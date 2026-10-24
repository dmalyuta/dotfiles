#!/bin/bash
#
# Set up a fresh CachyOS install. On a new machine, run
#
#   curl -fsSL https://raw.githubusercontent.com/dmalyuta/dotfiles/cachyos/start_fresh.sh | bash
#
# Safe to re-run: every step skips what is already done. With -c, reuse the answers from the last
# run instead of asking again (pass it through curl with `| bash -s -- -c`).
#
# This script installs and configures the software. At the end it runs scripts/configure_hardware.sh
# for low-level hardware setup (GPUs, kernel, sleep and hibernation, laptop quirks) and
# scripts/configure_kde.sh for the desktop.
#
# Author: Danylo Malyuta, 2026.

repo_ssh=git@github.com:dmalyuta/dotfiles.git
raw_base=https://raw.githubusercontent.com/dmalyuta/dotfiles/cachyos

# --------------------------------------------------------------------------------------------------
# Bootstrap.
# --------------------------------------------------------------------------------------------------

# Under `curl ... | bash` stdin is the script itself, so `read` would eat it. Re-exec a downloaded
# copy with the terminal on stdin instead.
if [ ! -t 0 ] && [ -z "${DOTFILES_BOOTSTRAP:-}" ]; then
	self=$(mktemp)
	curl -fsSL "$raw_base/start_fresh.sh" -o "$self" </dev/tty || exit
	DOTFILES_BOOTSTRAP=$self exec bash "$self" "$@" </dev/tty
fi
[ -n "${DOTFILES_BOOTSTRAP:-}" ] && trap 'rm -f "$DOTFILES_BOOTSTRAP"' EXIT

use_cached=
while getopts c opt; do
	case $opt in
	c) use_cached=1 ;;
	*) echo "Usage: $0 [-c]" >&2 && exit 1 ;;
	esac
done

# Use this checkout if running from one, else clone to ~/Projects/dotfiles below.
mkdir -p ~/Projects
dotfiles=~/Projects/dotfiles
this_script_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
if [ -f "$this_script_dir/home/.bash_aliases" ] && [ -e "$this_script_dir/.git" ]; then
	dotfiles=$this_script_dir
fi

downloads=~/Downloads
mkdir -p "$downloads" ~/.local/bin ~/.local/share/applications
cd "$downloads" || exit

# --------------------------------------------------------------------------------------------------
# Helpers.
# --------------------------------------------------------------------------------------------------

# Shared helpers, see scripts/functions.sh. Before the repo is cloned, fetch them from GitHub.
if [ "$dotfiles" = "$this_script_dir" ]; then
	. "$dotfiles"/scripts/functions.sh
else
	functions=$(curl -fsSL "$raw_base/scripts/functions.sh") || exit
	eval "$functions"
fi

# Yes/no question whose default is the last answer, remembered under key $1. With -c, a remembered
# answer is used without asking.
ask() {
	local key=$1 hint="[yN]" a
	if [ -n "$use_cached" ] && [ -n "${answers[$key]:-}" ]; then
		[ "${answers[$key]}" = y ]
		return
	fi
	[ "${answers[$key]:-}" = y ] && hint="[Yn]"
	read -p "$2 $hint " -r a
	[[ ${a:-${answers[$key]:-n}} =~ ^[Yy]$ ]] && answers[$key]=y || answers[$key]=n
	[ "${answers[$key]}" = y ]
}

# Whether $1 lists VS Code workspaces as "<letter>:<absolute path>,..." (or is empty), with distinct
# letters.
valid_workspaces() {
	local entry="[A-Z]:/[^,;]*[^,;/]"
	[[ $1 =~ ^($entry(,$entry)*)?$ ]] &&
		[ -z "$(tr , '\n' <<<"$1" | cut -c1 | sort | uniq -d)" ]
}

# --------------------------------------------------------------------------------------------------
# Questions, all up front so the rest runs unattended.
# --------------------------------------------------------------------------------------------------

# Last answers, kept in the repo (git-ignored). On a fresh install the repo is cloned later, so they
# are saved after that.
answers_file=$dotfiles/.start_fresh_answers
declare -A answers=()
[ -f "$answers_file" ] && . "$answers_file"

[ -n "$(git config --global user.name)" ] ||
	{ read -p "Git name: " -r a && git config --global user.name "$a"; }
[ -n "$(git config --global user.email)" ] ||
	{ read -p "Git email: " -r a && git config --global user.email "$a"; }

superluminal_dir=~/.local/superluminal
[ -x "$superluminal_dir"/Superluminal ] ||
	{ ask superluminal "Install Superluminal profiler?" && want_superluminal=1; }

if installed coolercontrol; then
	want_coolercontrol=1
elif ask coolercontrol "Install CoolerControl?"; then
	want_coolercontrol=1
	ask nct6775 "Try to find fans on desktop (nct6775)?" && want_nct6775=1
	if ask cc_backup "Restore CoolerControl settings from backup?"; then
		while true; do
			read -e -p "Path to the CoolerControl backup file: " -r cc_backup
			cc_backup="${cc_backup/#\~/$HOME}"
			[ -f "$cc_backup" ] && break
			echo "$cc_backup not found, try again."
		done
	fi
fi

installed openrgb || ask openrgb "Install OpenRGB?" && want_openrgb=1
# Also taken when already installed, so an install from another repo moves to the OGC one.
installed asusctl || ask asusctl "Install asusctl and ROG Control Center?" && want_asusctl=1
installed cardwire || ask cardwire "Install cardwire hybrid GPU manager?" && want_cardwire=1
installed brother-mfc-j805dw || ask brother "Install Brother printer driver?" && want_brother=1
ask obs "Install the custom OBS configuration?" && want_obs=1

pdfx_dir=~/.wine/drive_c/"Program Files/PDF-XChange"
[ -d "$pdfx_dir" ] || { ask pdfx "Install PDF-XChange Editor?" && want_pdfx=1; }

# Kept out of the repo, since the projects may be private. The last answer, or the rejected one on a
# retry, is pre-filled for editing.
if [ -z "$use_cached" ] || ! valid_workspaces "${answers[workspaces]-x}"; then
	echo "VS Code workspaces for Meta+C, Meta+<letter>, as <letter>:<absolute path>,... (empty for"
	echo "none). Meta+<letter> is taken from whatever else uses it."
	workspaces=${answers[workspaces]:-}
	while true; do
		read -e -i "$workspaces" -p "Workspaces: " -r workspaces || exit
		valid_workspaces "$workspaces" && break
		echo "Invalid: expected e.g. M:/home/me/project,X:/home/me/other, with distinct letters and"
		echo "no trailing slashes."
	done
	answers[workspaces]=$workspaces
fi

# --------------------------------------------------------------------------------------------------
# Packages.
# --------------------------------------------------------------------------------------------------

pacman_apps=(
	base-devel git curl wget unzip openssh paru
	bat btop htop tree fzf fd ripgrep shfmt gping
	kitty tmux ttf-cascadia-code-nerd
	proton-pass obsidian brave-bin okular inkscape obs-studio gimp
	vlc vlc-plugins-all flameshot nextcloud-client
	qalculate-gtk speedcrunch
	texlive perl-yaml-tiny perl-file-homedir
	openrazer-daemon python-openrazer keyd
	flatpak rustup nodejs npm grafana wine winetricks bazelisk
)
aur_apps=(
	visual-studio-code-bin polychromatic oh-my-posh-bin fsearch pureref otf-sn-pro
	xnviewmp nordvpn-bin nordvpn-gui-bin raddebugger
)
flatpak_apps=(
	com.github.tchx84.Flatseal
	io.github.tanaybhomia.Whisp
	io.github.amit9838.mousam
)

groups=(plugdev nordvpn)
services=(grafana nordvpnd keyd)

[ -n "${want_coolercontrol:-}" ] && pacman_apps+=(coolercontrol) && services+=(coolercontrold)
# The PX13 folds into a tablet, which needs an on-screen keyboard.
is_laptop && pacman_apps+=(plasma-keyboard)
[ -n "${want_openrgb:-}" ] && pacman_apps+=(openrgb i2c-tools) && groups+=(i2c)
[ -n "${want_asusctl:-}" ] && pacman_apps+=(asusctl rog-control-center) && services+=(asusd)
# For scripts/asus-power-limits.
[ -n "${want_asusctl:-}" ] && is_laptop && pacman_apps+=(acpi_call-dkms)
[ -n "${want_cardwire:-}" ] && pacman_apps+=(cardwire) && services+=(cardwired)
[ -n "${want_brother:-}" ] && pacman_apps+=(cups) && aur_apps+=(brother-mfc-j805dw) && services+=(cups)

# OpenGamingCollective's repo, for asusctl and cardwire straight from upstream. It goes first
# because pacman takes a package from the first repo that has it, and extra has asusctl too.
ogc_key=F79100EF8C802DAB81C323BB8EEA5962FE510E19
if [ -n "${want_asusctl:-}${want_cardwire:-}" ]; then
	if ! sudo pacman-key --list-keys "$ogc_key" >/dev/null 2>&1; then
		sudo pacman-key --recv-keys "$ogc_key"
		sudo pacman-key --lsign-key "$ogc_key"
	fi
	grep -q '^\[ogc\]' /etc/pacman.conf ||
		awk '!done && /^\[/ && !/^\[options\]/ {
			print "[ogc]\nServer = https://pacman.opengamingcollective.org\n"; done = 1
		} 1' /etc/pacman.conf | write_root_file /etc/pacman.conf
fi

sudo pacman -Syu --needed --noconfirm "${pacman_apps[@]}"
paru -S --needed --noconfirm --skipreview --sudoloop "${aur_apps[@]}"

# Bash as the login shell (CachyOS defaults to fish).
[[ $(getent passwd "$USER" | cut -d: -f7) == */bash ]] || sudo chsh -s /usr/bin/bash "$USER"

for g in "${groups[@]}"; do
	getent group "$g" >/dev/null || sudo groupadd --system "$g"
	sudo usermod -aG "$g" "$USER"
done

rustup toolchain list 2>/dev/null | grep -q stable || rustup default stable

# Flatpak apps, plus the GL extension matching the Nvidia driver so they get hardware rendering
# under Wayland.
flatpak remote-add --if-not-exists flathub https://dl.flathub.org/repo/flathub.flatpakrepo
nvidia_version=$(modinfo -F version nvidia 2>/dev/null)
[ -n "$nvidia_version" ] &&
	flatpak_apps+=("org.freedesktop.Platform.GL.nvidia-${nvidia_version//./-}")
flatpak install -y --noninteractive flathub "${flatpak_apps[@]}"

# Superluminal: a plain tarball, unpacked user-owned so its updater can write.
if [ -n "${want_superluminal:-}" ]; then
	superluminal_url=$(curl -fsSL https://superluminal.eu/download/ |
		grep -oE 'https://[^"]*/SuperluminalLinux-[^"]*\.tar\.gz' | head -n 1)
	if [ -z "$superluminal_url" ]; then
		say "Could not find the Superluminal download link, skipping." >&2
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

# --------------------------------------------------------------------------------------------------
# SSH and dotfiles.
# --------------------------------------------------------------------------------------------------

mkdir -p -m 700 ~/.ssh
[ -f ~/.ssh/id_ed25519 ] || ssh-keygen -q -t ed25519 -C "$(git config --global user.email)" -f ~/.ssh/id_ed25519 -N ""
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
	git clone -b cachyos "$repo_ssh" "$dotfiles"
fi

git -C "$dotfiles" submodule update --init --recursive
declare -p answers >"$answers_file"
mkdir -p ~/.config/kitty ~/.config/tmux-powerline/themes
ln -sf "$dotfiles"/home/{.bash_aliases,.local.bashrc,.dircolors,.wezterm.lua,.alacritty.toml} ~
ln -sf "$dotfiles"/home/.tmux.conf ~
ln -sfn "$dotfiles"/bin ~/.bin
ln -sf "$dotfiles"/config/kitty/{kitty.conf,default-kitty,resize_split.py} ~/.config/kitty
ln -sf "$dotfiles"/config/tmux-powerline/config.sh ~/.config/tmux-powerline/
ln -sf "$dotfiles"/config/tmux-powerline/themes/danylo-theme.sh ~/.config/tmux-powerline/themes/
ln -sf "$dotfiles"/config/.blue-owl-custom.omp.json ~
compgen -G ~/'.local/share/fonts/caskaydiacove*' >/dev/null || oh-my-posh font install CascadiaCode
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

# --------------------------------------------------------------------------------------------------
# Per-app configuration.
# --------------------------------------------------------------------------------------------------

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

if [ -n "${want_obs:-}" ]; then
	# OBS profile and scene collection, see config/obs/. Linked by directory, since OBS saves a file
	# by renaming a new one over it, which would replace a linked file. Existing ones are kept as
	# *.orig.
	obs_basic=~/.config/obs-studio/basic
	mkdir -p "$obs_basic"
	for d in profiles scenes; do
		[ -d "$obs_basic/$d" ] && [ ! -L "$obs_basic/$d" ] && mv -T "$obs_basic/$d" "$obs_basic/$d.orig"
		ln -sfn "$dotfiles/config/obs/$d" "$obs_basic/$d"
	done
	# Use that profile and scene collection, else OBS makes empty "Untitled" ones in the repo.
	# FirstRun skips the first-run auto-configuration wizard, which would overwrite the profile.
	obs_user=~/.config/obs-studio/user.ini
	kwriteconfig6 --file "$obs_user" --group General --key FirstRun true
	kwriteconfig6 --file "$obs_user" --group Basic --key Profile desktop
	kwriteconfig6 --file "$obs_user" --group Basic --key ProfileDir desktop
	kwriteconfig6 --file "$obs_user" --group Basic --key SceneCollection desktop_monitor
	kwriteconfig6 --file "$obs_user" --group Basic --key SceneCollectionFile desktop_monitor.json
	# Echo-cancelled webcam mic, so calls played on the speakers don't leak into OBS's mic.
	echo_cancel=~/.config/pipewire/pipewire.conf.d/60-echo-cancel.conf
	if [ "$(readlink "$echo_cancel")" != "$dotfiles/config/obs/pipewire/60-echo-cancel.conf" ]; then
		mkdir -p "${echo_cancel%/*}"
		ln -sf "$dotfiles"/config/obs/pipewire/60-echo-cancel.conf "$echo_cancel"
		systemctl --user restart pipewire pipewire-pulse wireplumber
	fi
	# The webcam mic level that the scene's filters are tuned for. PipeWire takes a moment to bring
	# the device back after a restart.
	if [ -e /proc/asound/C920 ]; then
		for try in {1..20}; do
			pactl set-source-volume alsa_input.usb-046d_HD_Pro_Webcam_C920_035EF47F-02.analog-stereo 80% \
				2>/dev/null && break
			sleep 0.5
		done
	fi
fi

# Grafana needs to read /home. Do NOT chmod -R here.
chmod o+rx "$HOME"
if write_root_file /etc/systemd/system/grafana.service.d/override.conf <<'EOF'
[Service]
ProtectHome=false
EOF
then
	sudo systemctl daemon-reload
	sudo systemctl try-restart grafana
fi

# keyd remaps keys below the display server, see scripts/keyd.conf. A restart applies a new config.
install_script_file keyd.conf /etc/keyd/default.conf && sudo systemctl try-restart keyd

sudo systemctl enable --now "${services[@]}"

# Batch mode accepts the license and skips the prompts, including the final one that sets up
# ~/.bashrc. Good: .local.bashrc sets conda up instead, because the block `conda init` writes delays
# every prompt by ~1 s. Remove that block if it was added before.
if [ -d ~/anaconda3 ]; then
	skip "Anaconda"
else
	curl -fL -o anaconda.sh \
		"https://repo.anaconda.com/archive/Anaconda3-2026.07-1-Linux-x86_64.sh" &&
		bash anaconda.sh -b -p ~/anaconda3
fi
sed -i '/^# >>> conda initialize >>>$/,/^# <<< conda initialize <<<$/d' ~/.bashrc

[ -f ~/.wine/drive_c/windows/Fonts/arial.ttf ] || winetricks -q corefonts
if [ -n "${want_pdfx:-}" ]; then
	while [ ! -f EditorV11.x64.msi ]; do
		echo "Download PDF-XChange Editor Plus 64-bit MSI installer from"
		echo "https://www.pdf-xchange.com/product/downloads into $downloads"
		read -p "Press ENTER to try again... " -r
	done
	wine msiexec /i EditorV11.x64.msi
fi

# Ship our own top-level launcher that runs the exe directly, and hide Wine's. This makes sure
# that the icon can be pinned to the task manager etc.
if [ -x "$pdfx_dir/PDF Editor/PXCEditor.exe" ]; then
	pdfx_icon=application-pdf
	for f in ~/.local/share/icons/hicolor/48x48/apps/*_PXCEditor.0.png; do
		[ -e "$f" ] && pdfx_icon=$(basename "$f" .png) && break
	done
	cat >~/.local/share/applications/pdf-xchange-editor.desktop <<EOF
[Desktop Entry]
Type=Application
Name=PDF-XChange Editor
GenericName=PDF Editor
Comment=View and edit PDF documents
Exec=env WINEPREFIX=$HOME/.wine WINEDEBUG=fixme-all wine "$pdfx_dir/PDF Editor/PXCEditor.exe" %f
Icon=$pdfx_icon
Terminal=false
StartupNotify=true
StartupWMClass=pxceditor.exe
Categories=Office;Viewer;
MimeType=application/pdf;
EOF
	set_desktop_key ~/.local/share/applications/wine/Programs/PDF-XChange/"PDF-XChange Editor.desktop" \
		NoDisplay true
	update-desktop-database ~/.local/share/applications
	kbuildsycoca6 >/dev/null 2>&1
fi

# Silence Wine from logging the spammy "fixme:" channel to stderr. Keep "err:" and "warn:", so real
# faults get reported.
mkdir -p ~/.config/environment.d
cat >~/.config/environment.d/50-winedebug.conf <<'EOF'
# Silence Wine's "fixme:" chatter, which otherwise floods the systemd journal.
# Real problems still log: the err: and warn: channels are left enabled.
WINEDEBUG=fixme-all
EOF
# environment.d only applies to sessions started after it, so cover the launchers directly too.
while IFS= read -r -d '' f; do
	grep -q '^Exec=env .*wine' "$f" && ! grep -q WINEDEBUG "$f" &&
		sed -i 's|^Exec=env |Exec=env "WINEDEBUG=fixme-all" |' "$f"
done < <(find ~/.local/share/applications -name '*.desktop' -print0 2>/dev/null)

# Flameshot's Wayland clipboard copy is lost on non-Gnome desktops because the capture window closes
# too early. Run the daemon under XWayland instead, for both ways it gets started: autostart and
# D-Bus activation.
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

# Explicitly set the journald size (default 50 MB).
if write_root_file /etc/systemd/journald.conf.d/99-size.conf <<'EOF'
[Journal]
SystemMaxUse=256M
EOF
then
	sudo systemctl restart systemd-journald
fi

# Low-level hardware setup (GPUs, kernel, sleep and hibernation, laptop quirks), see
# scripts/configure_hardware.sh.
bash "$dotfiles"/scripts/configure_hardware.sh
[ $? -eq 2 ] && need_reboot=1

# KDE desktop configuration, see scripts/configure_kde.sh.
bash "$dotfiles"/scripts/configure_kde.sh "${answers[workspaces]:-}"
[ $? -eq 2 ] && need_reboot=1

if [ -z "${need_reboot:-}" ]; then
	say "Done."
else
	read -r -s -p "$(say "Done. Press ENTER to reboot so the changes take effect...")"
	echo
	sudo reboot
fi
