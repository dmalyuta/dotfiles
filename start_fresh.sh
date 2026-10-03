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
dbus_call() {
	local dest=$1 path=$2 method=$3
	shift 3
	gdbus call --session --dest "$dest" --object-path "$path" --method "$method" "$@"
}

kwin() {
	dbus_call org.kde.KWin "$@" >/dev/null
}

plasma_script() {
	dbus_call org.kde.plasmashell /PlasmaShell org.kde.PlasmaShell.evaluateScript "$1"
}

# Ids of the given plasma objects. gdbus prints the returned string as ('...',).
plasma_ids() {
	plasma_script "print($1.map(x => x.id).join(' '))" | sed -E "s/^\('(.*)',\)$/\1/"
}

# Config group of the panel applet with the given plugin, printed as "containment applet".
applet_group() {
	awk -F'[][]' -v want="plugin=$1" '
		/^\[Containments]\[[0-9]+]\[Applets]\[[0-9]+]$/ { c = $4; a = $8 }
		$0 == want { print c, a; exit }
	' ~/.config/plasma-org.kde.plasma.desktop-appletsrc
}

# Qt key code of a "Mod+Mod+Key" binding, as kglobalaccel's D-Bus API takes it.
qt_keycode() {
	local part code=0
	local -A codes=([Shift]=0x02000000 [Ctrl]=0x04000000 [Alt]=0x08000000
		[Meta]=0x10000000 [Tab]=0x01000001 [Print]=0x01000009 [Del]=0x01000007 [Left]=0x01000012
		[Up]=0x01000013 [Right]=0x01000014 [None]=0)
	for part in ${1//+/ }; do
		if [[ $part == [A-Z] ]]; then
			((code += $(printf %d "'$part")))
		elif [ -n "${codes[$part]:-}" ]; then
			((code += codes[$part]))
		else
			echo "Unknown key '$part' in '$1'" >&2
			return 1
		fi
	done
	echo "$code"
}

# Remove the key with Qt key code $1 from the actions that have it, except action $3 of component
# $2, keeping their other keys. kglobalaccel does not let two actions share a key.
take_key() {
	local action action_name component component_name keys
	while IFS='|' read -r action action_name component component_name keys; do
		[ "$component|$action" = "$2|$3" ] && continue
		keys=$(tr -d ' ' <<<"$keys" | tr , '\n' | grep -vx "$1" | paste -sd ,)
		dbus_call org.kde.kglobalaccel /kglobalaccel org.kde.KGlobalAccel.setForeignShortcut \
			"['$component','$action','$component_name','$action_name']" "[${keys:-0}]" >/dev/null
	done < <(dbus_call org.kde.kglobalaccel /kglobalaccel org.kde.KGlobalAccel.globalShortcutsByKey \
		"([$1],)" "(0,)" | grep -oP "\('[^']*', '[^']*', '[^']*', '[^']*', '[^']*', '[^']*', \[[^]]*\]" |
		sed -E "s/^\('([^']*)', '([^']*)', '([^']*)', '([^']*)', '[^']*', '[^']*', \[(.*)\]$/\1|\2|\3|\4|\5/")
}

# Bind a global shortcut, taking the key from any other action that has it. Args: component, its
# friendly name, action, its friendly name, keys (see qt_keycode, or None to unbind).
set_shortcut() {
	local id code try
	# Skip on error: an empty key list would unbind the action.
	code=$(qt_keycode "$5") || return
	id="['$1','$3','$2','$4']"
	[ "$code" -ne 0 ] && take_key "$code" "$1" "$3"
	# kglobalaccel ignores a desktop entry's shortcut until it notices the entry, which for a new one
	# takes a few seconds after kbuildsycoca6, so retry while it is missing.
	for try in {1..20}; do
		dbus_call org.kde.kglobalaccel /kglobalaccel org.kde.KGlobalAccel.doRegister "$id" >/dev/null
		dbus_call org.kde.kglobalaccel /kglobalaccel org.kde.KGlobalAccel.setForeignShortcut \
			"$id" "[$code]" >/dev/null
		[[ $1 == *.desktop ]] && [ -f ~/.local/share/applications/"$1" ] || return 0
		dbus_call org.kde.kglobalaccel "/component/${1//[^A-Za-z0-9]/_}" \
			org.kde.kglobalaccel.Component.shortcutNames 2>/dev/null | grep -qF "'$3'" && return
		sleep 0.5
	done
	echo "Could not register the $2 shortcut." >&2
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
	flatpak rustup nodejs npm grafana wine winetricks
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
# ~/.bashrc, so do that part with conda init.
if [ -d ~/anaconda3 ]; then
	skip "Anaconda"
else
	curl -fL -o anaconda.sh \
		"https://repo.anaconda.com/archive/Anaconda3-2026.07-1-Linux-x86_64.sh" &&
		bash anaconda.sh -b -p ~/anaconda3
fi
grep -qF 'conda initialize' ~/.bashrc 2>/dev/null || ~/anaconda3/bin/conda init bash

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

# Replug the C920 webcam when its USB link fails, see scripts/c920-guard.
install_script_file c920-guard.service /etc/systemd/system/c920-guard.service && sudo systemctl daemon-reload
install_script_file c920-guard /usr/lib/systemd/system-sleep/c920-guard 0755
install_script_file c920-guard /usr/local/bin/c920-guard 0755 && sudo systemctl try-restart c920-guard
sudo systemctl enable --now c920-guard

# Explicitly set the journald size (default 50 MB).
if write_root_file /etc/systemd/journald.conf.d/99-size.conf <<'EOF'
[Journal]
SystemMaxUse=256M
EOF
then
	sudo systemctl restart systemd-journald
fi

# --------------------------------------------------------------------------------------------------
# KDE desktop configuration.
# --------------------------------------------------------------------------------------------------

# Reboot at the end only if these change. Writing an unchanged value leaves the file as is.
kde_files=(~/.config/{kcminputrc,kglobalshortcutsrc,kwinrc,ksmserverrc,kdeglobals,plasmashellrc}
	~/.config/plasma-org.kde.plasma.desktop-appletsrc)
kde_hash() { cat "${kde_files[@]}" 2>/dev/null | md5sum; }
kde_before=$(kde_hash)

# Input devices.
kconf --file kcminputrc --group Keyboard --key RepeatDelay 150
kconf --file kcminputrc --group Keyboard --key RepeatRate 50
kconf --file kcminputrc --group Mouse --key cursorSize 24
# Tell running apps the cursor changed (5 is KGlobalSettings::CursorChanged).
gdbus emit --session --object-path /KGlobalSettings \
	--signal org.kde.KGlobalSettings.notifyChange 5 0
# Libinput settings are per device, under [Libinput][vendor][product][name].
while IFS='|' read -r vendor product name; do
	device=(--file kcminputrc --group Libinput --group "$vendor" --group "$product" --group "$name")
	if [[ ${name,,} == *touchpad* ]]; then
		kconf "${device[@]}" --key NaturalScroll false
	else
		kconf "${device[@]}" --key PointerAcceleration -- -0.325
	fi
done < <(gawk -v RS= '/Handlers=[^\n]*mouse/ &&
	match($0, /Vendor=(\w+) Product=(\w+).*N: Name="([^"]*)"/, m) {
		print strtonum("0x" m[1]) "|" strtonum("0x" m[2]) "|" m[3]
	}' /proc/bus/input/devices)
compgen -G '/sys/class/backlight/*' >/dev/null &&
	say "If System Settings > Display has an auto-brightness toggle, turn it off."

# App hotkeys: a KWin script that focuses an app's window on any desktop, or launches the app. It
# launches through kglobalaccel, so each app it launches is registered below, with no key of its
# own.
mkdir -p ~/.local/share/kwin/scripts
ln -sfn "$dotfiles"/config/kwin/apphotkeys ~/.local/share/kwin/scripts/apphotkeys
kconf --file kwinrc --group Plugins --key apphotkeysEnabled true

# VS Code workspaces that Meta+C, Meta+<key> focuses, or opens if no window has them: this repo, and
# those from the question up front.
declare -A vscode_workspaces=([D]=$dotfiles)
IFS=, read -r -a entries <<<"${answers[workspaces]:-}"
for entry in "${entries[@]}"; do
	vscode_workspaces[${entry%%:*}]=${entry#*:}
done
# The script reads the list from its config on load, so unload it for Scripting.start (below) to load
# it again.
kwin /Scripting org.kde.kwin.Scripting.unloadScript apphotkeys
# A workspace's shortcuts and desktop entry are named by its letter. Drop those of letters no longer
# in the list, which frees their keys. Only those: re-registering a shortcut right after
# unregistering it is unreliable.
while read -r key; do
	[ -n "${vscode_workspaces[$key]:-}" ] && continue
	for id in "kwin App Hotkeys: VS Code workspace $key" "vscode-workspace-${key,,}.desktop _launch"; do
		dbus_call org.kde.kglobalaccel /kglobalaccel org.kde.KGlobalAccel.unregister \
			"${id%% *}" "${id#* }" >/dev/null
	done
	rm -f ~/.local/share/applications/vscode-workspace-"${key,,}".desktop
done < <({
	dbus_call org.kde.kglobalaccel /component/kwin org.kde.kglobalaccel.Component.shortcutNames |
		grep -oP "'App Hotkeys: VS Code workspace \K[A-Z]"
	dbus_call org.kde.kglobalaccel /kglobalaccel org.kde.KGlobalAccel.allComponents |
		grep -oP 'vscode_workspace_\K[a-z](?=_desktop)'
	ls ~/.local/share/applications | grep -oP '^vscode-workspace-\K[a-z](?=\.desktop$)'
} | tr a-z A-Z | sort -u)
# Each opens through a hidden desktop entry, so the script can launch it through kglobalaccel.
workspaces_config=
for key in "${!vscode_workspaces[@]}"; do
	dir=${vscode_workspaces[$key]}
	cat >~/.local/share/applications/vscode-workspace-"${key,,}".desktop <<EOF
[Desktop Entry]
Type=Application
Name=VS Code: ${dir##*/}
Exec=code "$dir"
Icon=vscode
NoDisplay=true
EOF
	workspaces_config+="$key=$dir;"
done
kconf --file kwinrc --group Script-apphotkeys --key vscodeWorkspaces "$workspaces_config"

# Global shortcuts. Set through kglobalaccel's D-Bus API because it keeps them in memory and
# overwrites edits to kglobalshortcutsrc. Apps are bound by desktop entry, where the "_launch"
# action runs its Exec, so first rebuild the cache that kglobalaccel finds the entries in. Each key
# is taken from KDE's default action for it, if any, e.g. Meta+Up from Quick Tile Top.
kbuildsycoca6 >/dev/null 2>&1
while IFS='|' read -r -a shortcut; do
	set_shortcut "${shortcut[@]}"
done <<'EOF'
kwin|KWin|Window Minimize|Minimize Window|Meta+Del
kwin|KWin|Window Maximize|Maximize Window|Meta+Up
kwin|KWin|Switch One Desktop to the Left|Switch One Desktop to the Left|Meta+Ctrl+Left
kwin|KWin|Switch One Desktop to the Right|Switch One Desktop to the Right|Meta+Ctrl+Right
kwin|KWin|Window One Desktop to the Left|Window One Desktop to the Left|Meta+Ctrl+Shift+Left
kwin|KWin|Window One Desktop to the Right|Window One Desktop to the Right|Meta+Ctrl+Shift+Right
kwin|KWin|Overview|Toggle Overview|Meta+Ctrl+Tab
kwin|KWin|Invert|Toggle Invert Effect|Ctrl+Shift+I
org.flameshot.Flameshot.desktop|Flameshot|Capture|Flameshot|Print
org.kde.konsole.desktop|Konsole|_launch|Konsole|None
kitty.desktop|kitty|_launch|kitty|Ctrl+Alt+T
brave-browser.desktop|Brave|_launch|Brave|None
org.kde.dolphin.desktop|Dolphin|_launch|Dolphin|None
io.github.cboxdoerfer.FSearch.desktop|FSearch|_launch|FSearch|None
pureref.desktop|PureRef|_launch|PureRef|None
obsidian.desktop|Obsidian|_launch|Obsidian|None
org.inkscape.Inkscape.desktop|Inkscape|_launch|Inkscape|None
speedcrunch.desktop|SpeedCrunch|_launch|SpeedCrunch|None
pdf-xchange-editor.desktop|PDF-XChange Editor|_launch|PDF-XChange Editor|None
com.microsoft.VSCode.desktop|Visual Studio Code|_launch|Visual Studio Code|None
kwin|KWin|App Hotkeys: Brave|App Hotkeys: Brave|Meta+B
kwin|KWin|App Hotkeys: New Brave window|App Hotkeys: New Brave window|Meta+Shift+B
kwin|KWin|App Hotkeys: Terminal|App Hotkeys: Terminal|Meta+W
kwin|KWin|App Hotkeys: New terminal|App Hotkeys: New terminal|Meta+Shift+W
kwin|KWin|App Hotkeys: Dolphin|App Hotkeys: Dolphin|Meta+E
kwin|KWin|App Hotkeys: New Dolphin window|App Hotkeys: New Dolphin window|Meta+Shift+E
kwin|KWin|App Hotkeys: FSearch|App Hotkeys: FSearch|Meta+S
kwin|KWin|App Hotkeys: PureRef|App Hotkeys: PureRef|Meta+R
kwin|KWin|App Hotkeys: Obsidian|App Hotkeys: Obsidian|Meta+O
kwin|KWin|App Hotkeys: Inkscape|App Hotkeys: Inkscape|Meta+I
kwin|KWin|App Hotkeys: SpeedCrunch|App Hotkeys: SpeedCrunch|Meta+N
kwin|KWin|App Hotkeys: PDF-XChange Editor|App Hotkeys: PDF-XChange Editor|Meta+P
kwin|KWin|App Hotkeys: VS Code|App Hotkeys: VS Code|Meta+C
EOF
for key in "${!vscode_workspaces[@]}"; do
	name="VS Code ${vscode_workspaces[$key]##*/}"
	set_shortcut vscode-workspace-"${key,,}".desktop "$name" _launch "$name" None
	set_shortcut kwin KWin "App Hotkeys: VS Code workspace $key" "App Hotkeys: $name" "Meta+$key"
done

# Virtual desktops, in one row. Existing ones are renamed rather than recreated so their windows
# stay put.
vdm() { kwin /VirtualDesktopManager "org.kde.KWin.VirtualDesktopManager.$1" "${@:2}"; }
desktop_names=(code browsing)
# The desktops property is a list of (position, id, name).
mapfile -t ids < <(dbus_call org.kde.KWin /VirtualDesktopManager \
	org.freedesktop.DBus.Properties.Get org.kde.KWin.VirtualDesktopManager desktops |
	grep -oP "\d+, '\K[^']+")
for i in "${!desktop_names[@]}"; do
	if [ -n "${ids[i]:-}" ]; then
		vdm setDesktopName "${ids[i]}" "${desktop_names[i]}"
	else
		vdm createDesktop "$i" "${desktop_names[i]}"
	fi
done
for id in "${ids[@]:${#desktop_names[@]}}"; do
	vdm removeDesktop "$id"
done
kwin /VirtualDesktopManager org.freedesktop.DBus.Properties.Set \
	org.kde.KWin.VirtualDesktopManager rows '<uint32 1>'

# Flash the desktop name for 200 ms on switch. This OSD is a KWin script, which a reconfigure does
# not load.
kconf --file kwinrc --group Plugins --key desktopchangeosdEnabled true
kconf --file kwinrc --group Script-desktopchangeosd --key PopupHideDelay 200
kconf --file kwinrc --group Script-desktopchangeosd --key TextOnly true
kwin /Scripting org.kde.kwin.Scripting.start

kconf --file ksmserverrc --group General --key loginMode emptySession
kconf --file ksmserverrc --group General --key confirmLogout false
kconf --file kdeglobals --group KDE --key AnimationDurationFactor 0
# Ctrl+Shift+I (set above) inverts the screen colors.
kconf --file kwinrc --group Plugins --key invertEnabled true
kwin /Effects org.kde.kwin.Effects.loadEffect invert
# Also unload the effects, since a reconfigure leaves running ones loaded.
for effect in wobblywindows magiclamp translucency squash scale fade glide \
	maximize fullscreen slide fadedesktop; do
	kconf --file kwinrc --group Plugins --key "${effect}Enabled" false
	kwin /Effects org.kde.kwin.Effects.unloadEffect "$effect"
done
kwin /KWin org.kde.KWin.reconfigure

laf=org.kde.breezedark.desktop
[ "$(kreadconfig6 --file kdeglobals --group KDE --key LookAndFeelPackage)" = "$laf" ] ||
	plasma-apply-lookandfeel --apply "$laf"

# Plasmashell only reads these settings on start, and writes its in-memory copy back over the file,
# so set them in the file and restart it if any changed. Args: file, kreadconfig6 --group/--key
# options, value.
restart_plasmashell=0
plasma_conf() {
	local file=$1 value=${!#} args=("${@:2:$#-2}")
	[ "$(kreadconfig6 --file "$file" "${args[@]}")" = "$value" ] && return
	kconf --file "$file" "${args[@]}" "$value"
	restart_plasmashell=1
}
# Panels 40 px tall and opaque (0 is adaptive, 1 opaque, 2 translucent).
for id in $(plasma_ids 'panels()'); do
	panel=(plasmashellrc --group PlasmaViews --group "Panel $id")
	plasma_conf "${panel[@]}" --group Defaults --key thickness 40
	plasma_conf "${panel[@]}" --key panelOpacity 1
done
# No desktop icons: switch from the Folder View layout to the plain one.
for id in $(plasma_ids 'desktops()'); do
	plasma_conf plasma-org.kde.plasma.desktop-appletsrc --group Containments --group "$id" \
		--key plugin org.kde.desktopcontainment
done
# Task manager launchers: exactly these, in this order. The whole list is rewritten, so anything
# else that was pinned is dropped.
read -r tm_cont tm_applet < <(applet_group org.kde.plasma.icontasks)
if [ -n "${tm_applet:-}" ]; then
	pins=(org.kde.dolphin brave-browser kitty com.microsoft.VSCode obsidian)
	[ -f ~/.local/share/applications/pdf-xchange-editor.desktop ] && pins+=(pdf-xchange-editor)
	list=$(printf ',applications:%s.desktop' "${pins[@]}")
	plasma_conf plasma-org.kde.plasma.desktop-appletsrc \
		--group Containments --group "$tm_cont" --group Applets --group "$tm_applet" \
		--group Configuration --group General --key launchers "${list#,}"
fi

[ "$restart_plasmashell" -eq 1 ] && systemctl --user restart plasma-plasmashell.service
# Kickoff favorites: only System Settings. They live in the activity manager's database rather
# than the applet config, so go through its D-Bus API; reading the database is safe while the
# daemon holds it open, but only in mode=ro, which sees writes still sitting in the WAL.
fav_agent=org.kde.plasma.favorites.applications
fav_want=applications:systemsettings.desktop
fav_link() {
	dbus_call org.kde.ActivityManager /ActivityManager/Resources/Linking \
		"org.kde.ActivityManager.ResourcesLinking.$1" "$fav_agent" "$2" :global >/dev/null
}
while read -r res; do
	[ -z "$res" ] || [ "$res" = "$fav_want" ] || fav_link UnlinkResourceFromActivity "$res"
done < <(sqlite3 "file:$HOME/.local/share/kactivitymanagerd/resources/database?mode=ro" \
	"select targettedResource from ResourceLink where initiatingAgent='$fav_agent';" 2>/dev/null)
fav_link LinkResourceToActivity "$fav_want"

# The shortcuts and OSD already show the current desktop, so drop the pager.
plasma_script 'panels().forEach(p =>
	p.widgets("org.kde.plasma.pager").forEach(w => w.remove()))' >/dev/null

if [ "$(kde_hash)" = "$kde_before" ]; then
	say "Done."
else
	read -r -s -p "$(say "Done. Press ENTER to reboot so the KDE changes take effect...")"
	echo
	sudo reboot
fi
