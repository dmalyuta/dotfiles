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

# Use this checkout if running from one, else clone to ~/sw/dotfiles below.
dotfiles=~/sw/dotfiles
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

# Qt key code of a "Mod+Mod+Key" binding, as kglobalaccel's D-Bus API takes it.
qt_keycode() {
	local part code=0
	local -A codes=([Shift]=0x02000000 [Ctrl]=0x04000000 [Alt]=0x08000000
		[Meta]=0x10000000 [Del]=0x01000007 [Left]=0x01000012 [Up]=0x01000013
		[Right]=0x01000014 [None]=0)
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
installed asusctl || ask asusctl "Install asusctl?" && want_asusctl=1
installed envycontrol || ask envycontrol "Install envycontrol hybrid GPU switching?" &&
	want_envycontrol=1
installed brother-mfc-j805dw || ask brother "Install Brother printer driver?" && want_brother=1

pdfx_dir=~/.wine/drive_c/"Program Files/PDF-XChange"
[ -d "$pdfx_dir" ] || { ask pdfx "Install PDF-XChange Editor?" && want_pdfx=1; }

# --------------------------------------------------------------------------------------------------
# Packages.
# --------------------------------------------------------------------------------------------------

repo=(
	base-devel git curl wget unzip openssh paru
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

# Flatpak apps, plus the GL extension matching the Nvidia driver so they get hardware rendering
# under Wayland.
flatpak remote-add --if-not-exists flathub https://dl.flathub.org/repo/flathub.flatpakrepo
flatpak_apps=(com.github.tchx84.Flatseal io.github.tanaybhomia.Whisp)
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

sudo systemctl enable --now "${services[@]}"

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

# Global shortcuts. Set through kglobalaccel's D-Bus API because it keeps them in memory and
# overwrites edits to kglobalshortcutsrc. Apps are bound by desktop entry, where the "_launch"
# action runs its Exec, so first rebuild the cache that kglobalaccel finds the entries in. Quick
# Tile Top has to release Meta+Up before Maximize can take it.
kbuildsycoca6 >/dev/null 2>&1
while IFS='|' read -r component component_name action action_name keys; do
	# Skip on error: an empty key list would unbind the action.
	code=$(qt_keycode "$keys") || continue
	id="['$component','$action','$component_name','$action_name']"
	dbus_call org.kde.kglobalaccel /kglobalaccel org.kde.KGlobalAccel.doRegister "$id" >/dev/null
	dbus_call org.kde.kglobalaccel /kglobalaccel org.kde.KGlobalAccel.setForeignShortcut \
		"$id" "[$code]" >/dev/null
done <<'EOF'
kwin|KWin|Window Minimize|Minimize Window|Meta+Del
kwin|KWin|Window Quick Tile Top|Quick Tile Window to the Top|None
kwin|KWin|Window Maximize|Maximize Window|Meta+Up
kwin|KWin|Switch One Desktop to the Left|Switch One Desktop to the Left|Meta+Ctrl+Left
kwin|KWin|Switch One Desktop to the Right|Switch One Desktop to the Right|Meta+Ctrl+Right
kwin|KWin|Window One Desktop to the Left|Window One Desktop to the Left|Meta+Ctrl+Shift+Left
kwin|KWin|Window One Desktop to the Right|Window One Desktop to the Right|Meta+Ctrl+Shift+Right
org.kde.konsole.desktop|Konsole|_launch|Konsole|None
kitty.desktop|kitty|_launch|kitty|Ctrl+Alt+T
org.flameshot.Flameshot.desktop|Flameshot|Capture|Flameshot|Shift+Ctrl+Alt+P
speedcrunch.desktop|SpeedCrunch|_launch|SpeedCrunch|Shift+Ctrl+Alt+N
brave-browser.desktop|Brave|_launch|Brave|Shift+Ctrl+Alt+B
pureref.desktop|PureRef|_launch|PureRef|Shift+Ctrl+Alt+R
obsidian.desktop|Obsidian|_launch|Obsidian|Shift+Ctrl+Alt+O
EOF

# Virtual desktops, in one row. Existing ones are renamed rather than recreated so their windows
# stay put.
vdm() { kwin /VirtualDesktopManager "org.kde.KWin.VirtualDesktopManager.$1" "${@:2}"; }
desktop_names=(code browsing windows)
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
[ "$restart_plasmashell" -eq 1 ] && systemctl --user restart plasma-plasmashell.service
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
