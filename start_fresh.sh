#!/bin/bash
#
# Set up a fresh CachyOS install. On a new machine, run
#
#   curl -fsSL https://raw.githubusercontent.com/dmalyuta/dotfiles/cachyos/start_fresh.sh | bash
#
# Safe to re-run: every step skips what is already done. With -c, reuse the answers from the last
# run instead of asking again (pass it through curl with `| bash -s -- -c`).
#
# Author: Danylo Malyuta, 2026.

repo_ssh=git@github.com:dmalyuta/dotfiles.git
raw_url=https://raw.githubusercontent.com/dmalyuta/dotfiles/cachyos/start_fresh.sh

# --------------------------------------------------------------------------------------------------
# Bootstrap.
# --------------------------------------------------------------------------------------------------

# Under `curl ... | bash` stdin is the script itself, so `read` would eat it. Re-exec a downloaded
# copy with the terminal on stdin instead.
if [ ! -t 0 ] && [ -z "${DOTFILES_BOOTSTRAP:-}" ]; then
	self=$(mktemp)
	curl -fsSL "$raw_url" -o "$self" </dev/tty || exit
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
if [ -f "$this_script_dir/home/.bash_aliases" ] && [ -d "$this_script_dir/.git" ]; then
	dotfiles=$this_script_dir
fi

downloads=~/Downloads
mkdir -p "$downloads" ~/.local/bin ~/.local/share/applications
cd "$downloads" || exit

# --------------------------------------------------------------------------------------------------
# Helpers.
# --------------------------------------------------------------------------------------------------

# Status message, in bold bright blue so it stands out.
say() { printf '\e[1;94m== %s ==\e[0m\n' "$*"; }

skip() { say "$* already present, skipping."; }

installed() { pacman -Q "$1" >/dev/null 2>&1; }

# Whether this is the laptop rather than the desktop: it has a system battery. A wireless mouse's or
# pen's battery also shows up, but with scope Device.
is_laptop() {
	local p
	for p in /sys/class/power_supply/*; do
		[ "$(cat "$p/type" 2>/dev/null)" = Battery ] &&
			[ "$(cat "$p/scope" 2>/dev/null)" != Device ] && return
	done
	return 1
}

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

kconf() {
	# --notify makes running programs reload the setting.
	kwriteconfig6 --notify "$@"
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
[ -n "${want_openrgb:-}" ] && pacman_apps+=(openrgb i2c-tools) && groups+=(i2c)
[ -n "${want_asusctl:-}" ] && pacman_apps+=(asusctl rog-control-center) && services+=(asusd)
# For scripts/asus-power-limits.
[ -n "${want_asusctl:-}" ] && [ "$(cat /sys/class/dmi/id/board_name 2>/dev/null)" = HN7306WU ] &&
	pacman_apps+=(acpi_call-dkms)
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

# Also hide /dev/nvidiactl etc. when the dGPU is blocked, so tools like nvtop don't wake it. Only
# reliable with exactly one iGPU and one Nvidia dGPU.
[ -n "${want_cardwire:-}" ] && lspci -d 10de: | grep -qE 'VGA|3D' &&
	cardwire config experimental-nvidia-block true

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

# Wake sources, and a working display after suspend. systemd only scans
# /usr/lib/systemd/system-sleep, not /etc.
if install_script_file 90-usb-wakeup.rules /etc/udev/rules.d/90-usb-wakeup.rules; then
	sudo udevadm control --reload
	sudo udevadm trigger --action=add --subsystem-match=usb
fi
install_script_file usb-wakeup /usr/lib/systemd/system-sleep/usb-wakeup 0755

# Zen 5's 32-bit RDSEED is broken until a microcode fix. Without it, the kernel turns RDSEED off
# (harmless) and prints "RDSEED32 is broken. Please update your firmware." on screen at every boot.
# The fix ships in amd-ucode, but the CPU only accepts it once a BIOS with AMD's EntrySign fix is
# installed, so loading it from Linux fails silently. Until then, hide the message: it is logged at
# emergency level, which only loglevel=0 keeps off the console. Panics and oopses still show,
# since they raise the console log level themselves.
if journalctl -k -b 0 -q -g 'RDSEED32 is broken' >/dev/null 2>&1 &&
	! grep -q 'loglevel=0' /etc/default/limine; then
	sudo sed -i -E -e 's/ microcode\.amd_sha_check=off//' \
		-e 's/^(KERNEL_CMDLINE\[default\]\+?=".*)"$/\1 loglevel=0"/' /etc/default/limine
	rebuild_initramfs=1
fi

# Laptop sleep. Its only sleep state is s2idle, which reaches the SoC's deepest state only if every
# device powers off, and the Nvidia driver keeps the dGPU on unless told otherwise, draining ~1.5%/h.
#
# Also keep Nvidia out of the initramfs, where chwd puts it. Resuming from hibernation happens in the
# initramfs, and the kernel must freeze its drivers before jumping into the image, which the freshly
# loaded Nvidia driver refuses (nv_pmops_freeze returns -EIO), so the image is dropped and the boot
# is fresh. The panel is on the iGPU, so the dGPU can load later. chwd also drops the kms hook, which
# the iGPU needs for an early splash, so add it back; it only picks in-tree drivers, not nvidia.
if is_laptop && [ -n "$nvidia_version" ]; then
	echo 'options nvidia NVreg_EnableS0ixPowerManagement=1' |
		write_root_file /etc/modprobe.d/nvidia-s0ix.conf && rebuild_initramfs=1
	write_root_file /etc/mkinitcpio.conf.d/90-no-nvidia.conf <<'EOF' && rebuild_initramfs=1
# Hibernation can't resume with Nvidia loaded in the initramfs, see start_fresh.sh.
MODULES=(${MODULES[@]/#nvidia*/})
HOOKS=(${HOOKS[@]/modconf/modconf kms})
EOF
	# Also rebuild if a past rebuild was skipped or failed, so an initramfs still has Nvidia.
	sudo find /boot -name 'initramfs-*' ! -name '*fallback*' -exec lsinitcpio {} \; 2>/dev/null |
		grep -q '/nvidia\.ko' && rebuild_initramfs=1
fi
# Also regenerates the boot entries, with the kernel command line from /etc/default/limine.
[ -n "${rebuild_initramfs:-}" ] && sudo limine-mkinitcpio

# Laptop hibernation, to a swapfile as big as RAM (zram can't hold the image). It goes in its own
# subvolume, since btrfs can't snapshot a subvolume with an active swapfile, and snapper snapshots
# @. The systemd initramfs finds it on resume through an EFI variable, so no resume= is needed.
swapfile=/swap/swapfile
if is_laptop && [ "$(findmnt -no FSTYPE /)" = btrfs ] && [ ! -f "$swapfile" ]; then
	root_dev=$(findmnt -no SOURCE / | sed 's/\[.*//')
	top=$(mktemp -d)
	sudo mount -o subvolid=5 "$root_dev" "$top"
	sudo btrfs subvolume show "$top/@swap" >/dev/null 2>&1 || sudo btrfs subvolume create "$top/@swap"
	sudo umount "$top" && rmdir "$top"
	{
		sudo cat /etc/fstab
		grep -q '[[:space:]]/swap[[:space:]]' /etc/fstab ||
			echo "UUID=$(findmnt -no UUID /) /swap btrfs subvol=/@swap,defaults,noatime 0 0"
		grep -q "^$swapfile[[:space:]]" /etc/fstab || echo "$swapfile none swap defaults 0 0"
	} | write_root_file /etc/fstab
	sudo systemctl daemon-reload
	sudo mkdir -p /swap
	mountpoint -q /swap || sudo mount /swap
	sudo btrfs filesystem mkswapfile --size "$(awk '/MemTotal/ { print int($2 / 1048576) + 1 }' \
		/proc/meminfo)g" "$swapfile"
	sudo swapon "$swapfile"
fi

# Sleep then hibernate after 3 h on battery, to cap the drain overnight. On AC it stays asleep, and
# the 3 h start when it is unplugged. SleepMode 3 is "Standby, then hibernate".
if is_laptop && [ -f "$swapfile" ]; then
	write_root_file /etc/systemd/sleep.conf.d/50-suspend-then-hibernate.conf <<'EOF'
[Sleep]
HibernateDelaySec=3h
HibernateOnACPower=no
EOF
	for profile in AC Battery LowBattery; do
		kconf --file powerdevilrc --group "$profile" --group SuspendAndShutdown --key SleepMode 3
	done
fi

# Replug the C920 webcam when its USB link fails, see scripts/c920-guard.
install_script_file c920-guard.service /etc/systemd/system/c920-guard.service && sudo systemctl daemon-reload
install_script_file c920-guard /usr/lib/systemd/system-sleep/c920-guard 0755
install_script_file c920-guard /usr/local/bin/c920-guard 0755 && sudo systemctl try-restart c920-guard
sudo systemctl enable --now c920-guard

# asusd sets the keyboard backlight, while systemd-backlight restores whatever it saved at
# shutdown, which can be 0. Also reset the keyboard controller after hibernation, see
# scripts/asus-kbd-rebind.
if installed asusctl; then
	sudo systemctl mask systemd-backlight@leds:asus::kbd_backlight.service
	install_script_file asus-kbd-rebind /usr/local/bin/asus-kbd-rebind 0755
	install_script_file asus-kbd-rebind.service /etc/systemd/system/asus-kbd-rebind.service &&
		sudo systemctl daemon-reload
	sudo systemctl enable asus-kbd-rebind
fi

# G-Helper's fan curves, see scripts/asus-fan-curves. asusd saves them.
if installed asusctl && [ "$(cat /sys/class/dmi/id/board_name 2>/dev/null)" = HN7306WU ]; then
	"$dotfiles"/scripts/asus-fan-curves >/dev/null
fi

# Per-profile CPU power limits, lower in Quiet for battery life, see scripts/asus-power-limits.
if installed acpi_call-dkms && [ "$(cat /sys/class/dmi/id/board_name 2>/dev/null)" = HN7306WU ]; then
	install_script_file asus-power-limits /usr/lib/systemd/system-sleep/asus-power-limits 0755
	install_script_file asus-power-limits.service /etc/systemd/system/asus-power-limits.service &&
		sudo systemctl daemon-reload
	install_script_file asus-power-limits /usr/local/bin/asus-power-limits 0755 &&
		sudo systemctl try-restart asus-power-limits
	sudo systemctl enable --now asus-power-limits
fi

# Explicitly set the journald size (default 50 MB).
if write_root_file /etc/systemd/journald.conf.d/99-size.conf <<'EOF'
[Journal]
SystemMaxUse=256M
EOF
then
	sudo systemctl restart systemd-journald
fi

# KDE desktop configuration, see scripts/configure_kde.sh.
bash "$dotfiles"/scripts/configure_kde.sh "${answers[workspaces]:-}"
if [ $? -ne 2 ]; then
	say "Done."
else
	read -r -s -p "$(say "Done. Press ENTER to reboot so the KDE changes take effect...")"
	echo
	sudo reboot
fi
