#!/bin/bash
#
# Set up a fresh CachyOS install. On a new machine, run
#
#   curl -fsSL https://raw.githubusercontent.com/dmalyuta/dotfiles/master/start_fresh.sh | bash
#
# Safe to re-run: every step skips what is already done.
#
# Author: Danylo Malyuta, 2026.

repo_ssh=git@github.com:dmalyuta/dotfiles.git
raw_url=https://raw.githubusercontent.com/dmalyuta/dotfiles/cachyos/start_fresh.sh

# ---------------------------------------------------------------------------
# Bootstrap.
# ---------------------------------------------------------------------------

# Under `curl ... | bash` stdin is the script itself, so `read` would eat it.
# Re-exec a downloaded copy with the terminal on stdin instead.
if [ ! -t 0 ] && [ -z "${DOTFILES_BOOTSTRAP:-}" ]; then
	self=$(mktemp)
	curl -fsSL "$raw_url" -o "$self" </dev/tty || exit
	DOTFILES_BOOTSTRAP=$self exec bash "$self" "$@" </dev/tty
fi
[ -n "${DOTFILES_BOOTSTRAP:-}" ] && trap 'rm -f "$DOTFILES_BOOTSTRAP"' EXIT

# Use this checkout if running from one, else clone to ~/sw/dotfiles below.
dotfiles=~/sw/dotfiles
this_script_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
if [ -f "$this_script_dir/home/.bash_aliases" ] && [ -d "$this_script_dir/.git" ]; then
	dotfiles=$this_script_dir
fi

downloads=~/Downloads
mkdir -p "$downloads" ~/.local/bin ~/.local/share/applications
cd "$downloads" || exit

# ---------------------------------------------------------------------------
# Helpers.
# ---------------------------------------------------------------------------

skip() { echo "== $* already present, skipping."; }

installed() { pacman -Q "$1" >/dev/null 2>&1; }

ask() {
	local a
	read -p "$1 [yN] " -r a
	[[ $a =~ ^[Yy]$ ]]
}

# Write stdin to a root-owned file. Returns 0 only if the content changed.
write_root_file() {
	local dest=$1 tmp
	tmp=$(mktemp)
	cat >"$tmp"
	if sudo cmp -s "$tmp" "$dest" 2>/dev/null; then
		rm -f "$tmp"
		return 1
	fi
	sudo install -Dm644 "$tmp" "$dest"
	rm -f "$tmp"
}

# Install a file from scripts/. Returns 0 only if the content changed.
install_script_file() {
	local src=$dotfiles/scripts/$1 dest=$2 mode=${3:-644}
	sudo cmp -s "$src" "$dest" 2>/dev/null && return 1
	sudo install -Dm "$mode" "$src" "$dest"
}

# Set a key in a .desktop file's [Desktop Entry] group.
set_desktop_key() {
	local file=$1 key=$2 value=$3 sudo_cmd=()
	[ -f "$file" ] || return 0
	[ -w "$file" ] || sudo_cmd=(sudo)
	if grep -q "^${key}=" "$file"; then
		"${sudo_cmd[@]}" sed -i "s|^${key}=.*|${key}=${value}|" "$file"
	else
		"${sudo_cmd[@]}" sed -i "/^\[Desktop Entry\]/a ${key}=${value}" "$file"
	fi
}

# ---------------------------------------------------------------------------
# Questions, all up front so the rest runs unattended.
# ---------------------------------------------------------------------------

[ -n "$(git config --global user.name)" ] ||
	{ read -p "Git name: " -r a && git config --global user.name "$a"; }
[ -n "$(git config --global user.email)" ] ||
	{ read -p "Git email: " -r a && git config --global user.email "$a"; }

superluminal_dir=~/.local/superluminal
[ -x "$superluminal_dir"/Superluminal ] || { ask "Install Superluminal profiler?" && want_superluminal=1; }

if installed coolercontrol; then
	want_coolercontrol=1
elif ask "Install CoolerControl?"; then
	want_coolercontrol=1
	ask "Try to find fans on desktop (nct6775)?" && want_nct6775=1
	if ask "Restore CoolerControl settings from backup?"; then
		while true; do
			read -e -p "Path to the CoolerControl backup file: " -r cc_backup
			cc_backup="${cc_backup/#\~/$HOME}"
			[ -f "$cc_backup" ] && break
			echo "$cc_backup not found, try again."
		done
	fi
fi

installed openrgb || ask "Install OpenRGB?" && want_openrgb=1
installed asusctl || ask "Install asusctl?" && want_asusctl=1
installed envycontrol || ask "Install envycontrol hybrid GPU switching?" && want_envycontrol=1
installed brother-mfc-j805dw || ask "Install Brother printer driver?" && want_brother=1

pdfx_dir=~/.wine/drive_c/"Program Files/Tracker Software"
[ -d "$pdfx_dir" ] || { ask "Install PDF-XChange Editor?" && want_pdfx=1; }

# ---------------------------------------------------------------------------
# Packages.
# ---------------------------------------------------------------------------

repo=(
	base-devel git curl wget unzip openssh
	bat btop htop tree fzf fd ripgrep shfmt
	kitty tmux ttf-cascadia-code-nerd
	proton-pass obsidian brave-bin okular inkscape obs-studio gimp
	vlc vlc-plugins-all flameshot nextcloud-client
	qalculate-gtk speedcrunch
	openrazer-daemon python-openrazer
	flatpak rustup nodejs npm grafana wine winetricks
)
aur=(
	visual-studio-code-bin polychromatic oh-my-posh-bin fsearch pureref
	xnviewmp nordvpn-bin nordvpn-gui-bin
)
groups=(plugdev nordvpn)
services=(grafana nordvpnd)

[ -n "${want_coolercontrol:-}" ] && repo+=(coolercontrol) && services+=(coolercontrold)
[ -n "${want_openrgb:-}" ] && repo+=(openrgb i2c-tools) && groups+=(i2c)
[ -n "${want_asusctl:-}" ] && repo+=(asusctl) && services+=(asusd)
[ -n "${want_envycontrol:-}" ] && aur+=(envycontrol)
[ -n "${want_brother:-}" ] && repo+=(cups) && aur+=(brother-mfc-j805dw) && services+=(cups)

sudo pacman -Syu --needed --noconfirm "${repo[@]}"
paru -S --needed --noconfirm --skipreview --sudoloop "${aur[@]}"

# Bash as the login shell (CachyOS defaults to fish).
[[ $(getent passwd "$USER" | cut -d: -f7) == */bash ]] || sudo chsh -s /usr/bin/bash "$USER"

for g in "${groups[@]}"; do
	getent group "$g" >/dev/null || sudo groupadd --system "$g"
	sudo usermod -aG "$g" "$USER"
done

rustup toolchain list 2>/dev/null | grep -q stable || rustup default stable

# Flatpak apps, plus the GL extension matching the Nvidia driver so they get
# hardware rendering under Wayland.
flatpak remote-add --if-not-exists flathub https://dl.flathub.org/repo/flathub.flatpakrepo
flatpak_apps=(com.github.tchx84.Flatseal io.github.tanaybhomia.Whisp)
nvidia_version=$(modinfo -F version nvidia 2>/dev/null)
[ -n "$nvidia_version" ] && flatpak_apps+=("org.freedesktop.Platform.GL.nvidia-${nvidia_version//./-}")
flatpak install -y --noninteractive flathub "${flatpak_apps[@]}"

# Superluminal: a plain tarball, unpacked user-owned so its updater can write.
if [ -n "${want_superluminal:-}" ]; then
	superluminal_url=$(curl -fsSL https://superluminal.eu/download/ |
		grep -oE 'https://[^"]*/SuperluminalLinux-[^"]*\.tar\.gz' | head -n 1)
	if [ -z "$superluminal_url" ]; then
		echo "== Could not find the Superluminal download link, skipping." >&2
	else
		mkdir -p "$superluminal_dir" ~/.local/share/icons/hicolor/scalable/apps
		# Entries are ./Superluminal/..., so strip both components.
		curl -fsSL "$superluminal_url" | tar xz -C "$superluminal_dir" --strip-components=2
		cp "$superluminal_dir"/Documentation/Superluminal/assets/img/logo.svg \
			~/.local/share/icons/hicolor/scalable/apps/superluminal.svg
		ln -sf "$superluminal_dir"/Superluminal ~/.local/bin/superluminal
		cat >~/.local/share/applications/superluminal.desktop <<EOF
[Desktop Entry]
Type=Application
Name=Superluminal
Comment=CPU profiler
Exec=$superluminal_dir/Superluminal
Icon=superluminal
Terminal=false
Categories=Development;Profiling;
EOF
	fi
fi

# ---------------------------------------------------------------------------
# SSH and dotfiles.
# ---------------------------------------------------------------------------

[ -f ~/.ssh/id_ed25519 ] || ssh-keygen -t ed25519 -C "$(git config --global user.email)"
eval "$(ssh-agent -s)"
ssh-add ~/.ssh/id_ed25519
ssh-keygen -F github.com >/dev/null 2>&1 || ssh-keyscan github.com >>~/.ssh/known_hosts 2>/dev/null

if [ -d "$dotfiles" ]; then
	skip "dotfiles repo"
else
	until ssh -T git@github.com </dev/null 2>&1 | grep -q 'successfully authenticated'; do
		echo -e "\nAdd this public key to https://github.com/settings/keys:\n"
		cat ~/.ssh/id_ed25519.pub
		read -p $'\nPress ENTER once it is added... ' -r
	done
	git clone "$repo_ssh" "$dotfiles"
fi

git -C "$dotfiles" submodule update --init --recursive
mkdir -p ~/.config/kitty ~/.config/tmux-powerline/themes
ln -sf "$dotfiles"/home/{.bash_aliases,.local.bashrc,.dircolors,.wezterm.lua,.alacritty.toml,.tmux.conf} ~
ln -sfn "$dotfiles"/bin ~/.bin
ln -sf "$dotfiles"/config/kitty/{kitty.conf,default-kitty,resize_split.py} ~/.config/kitty
ln -sf "$dotfiles"/config/tmux-powerline/config.sh ~/.config/tmux-powerline/
ln -sf "$dotfiles"/config/tmux-powerline/themes/danylo-theme.sh ~/.config/tmux-powerline/themes/
ln -sf "$dotfiles"/config/.blue-owl-custom.omp.json ~
echo 'kitty.desktop' >~/.config/xdg-terminals.list

[ -d ~/.tmux/plugins/tpm ] || git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm

if grep -qF 'oh-my-posh init bash' ~/.bashrc 2>/dev/null; then
	skip "bashrc block"
else
	cat >>~/.bashrc <<'EOF'

# Binaries.
export PATH=$PATH:~/.local/bin
export PATH=$PATH:~/.bin/git-custom-commands

# Enable fzf commands
eval "$(fzf --bash)"
# source ~/.bin/fzf-tab-completion/bash/fzf-bash-completion.sh
# bind -x '"\t": fzf_bash_completion'

# Oh-my-posh
eval "$(oh-my-posh init bash --config ~/.blue-owl-custom.omp.json)"

# Aliases.
if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# Custom bashrc setup.
if [ -f ~/.local.bashrc ]; then
    . ~/.local.bashrc
fi
EOF
fi

# ---------------------------------------------------------------------------
# Per-app configuration.
# ---------------------------------------------------------------------------

if [ -n "${want_nct6775:-}" ]; then
	sudo modprobe nct6775
	echo nct6775 | write_root_file /etc/modules-load.d/nct6775.conf
fi
[ -n "${cc_backup:-}" ] && sudo tar -xvf "$cc_backup" -C /

if [ -n "${want_openrgb:-}" ]; then
	sudo modprobe -a i2c-dev i2c-piix4
	printf 'i2c-dev\ni2c-piix4\n' | write_root_file /etc/modules-load.d/i2c.conf
	mkdir -p ~/.config/OpenRGB/profiles ~/.config/OpenRGB/plugins
	ln -sf "$dotfiles"/config/OpenRGB/{Configuration.json,OpenRGB.json} ~/.config/OpenRGB/
	ln -sf "$dotfiles"/config/OpenRGB/profiles/{blue.json,off.json} ~/.config/OpenRGB/profiles/
fi

# Grafana needs to read /home. Do NOT chmod -R here.
chmod o+rx "$HOME"
if write_root_file /etc/systemd/system/grafana.service.d/override.conf <<'EOF'; then
[Service]
ProtectHome=false
EOF
	sudo systemctl daemon-reload
	sudo systemctl try-restart grafana
fi

sudo systemctl enable --now "${services[@]}"

if [ -d ~/anaconda3 ]; then
	skip "Anaconda"
else
	curl -fL -o anaconda.sh "https://repo.anaconda.com/archive/Anaconda3-2026.07-1-Linux-x86_64.sh" &&
		bash anaconda.sh
fi

[ -f ~/.wine/drive_c/windows/Fonts/arial.ttf ] || winetricks -q corefonts
if [ -n "${want_pdfx:-}" ]; then
	while [ ! -f EditorV11.x64.msi ]; do
		echo "Download PDF-XChange Editor Plus 64-bit MSI installer from https://www.pdf-xchange.com/product/downloads into $downloads"
		read -p "Press ENTER to try again... " -r
	done
	wine msiexec /i EditorV11.x64.msi
fi

# Flameshot's Wayland clipboard copy is lost on non-Gnome desktops because the
# capture window closes too early. Run the daemon under XWayland instead, for
# both ways it gets started: autostart and D-Bus activation.
flameshot_autostart=~/.config/autostart/Flameshot.desktop
if [ ! -f "$flameshot_autostart" ]; then
	mkdir -p ~/.config/autostart
	cat >"$flameshot_autostart" <<'EOF'
[Desktop Entry]
Name=flameshot
Icon=flameshot
Exec=flameshot
Terminal=false
Type=Application
X-GNOME-Autostart-enabled=true
EOF
fi
set_desktop_key "$flameshot_autostart" Exec "env QT_QPA_PLATFORM=xcb flameshot"
mkdir -p ~/.local/share/dbus-1/services
cat >~/.local/share/dbus-1/services/org.flameshot.Flameshot.service <<'EOF'
[D-BUS Service]
Name=org.flameshot.Flameshot
Exec=/usr/bin/env QT_QPA_PLATFORM=xcb /usr/bin/flameshot
EOF

# Wake sources, and a working display after suspend. systemd only scans
# /usr/lib/systemd/system-sleep, not /etc.
if install_script_file 90-usb-wakeup.rules /etc/udev/rules.d/90-usb-wakeup.rules; then
	sudo udevadm control --reload
	sudo udevadm trigger --action=add --subsystem-match=usb
fi
install_script_file usb-wakeup /usr/lib/systemd/system-sleep/usb-wakeup 0755

set_desktop_key /usr/share/applications/matlab.desktop StartupWMClass "MATLAB R2026a Update 5"
set_desktop_key /usr/share/applications/matlab.desktop X-AppImage-Name "MATLAB R2026a"

# ---------------------------------------------------------------------------
# COSMIC desktop configuration.
# ---------------------------------------------------------------------------

# Copied, not linked: COSMIC replaces its config files on write. Only the keys
# that differ from the defaults are kept in the repo.
cp -r "$dotfiles"/config/cosmic/. ~/.config/cosmic/

echo "== Done. Log out and back in for the shell and group changes to apply."
