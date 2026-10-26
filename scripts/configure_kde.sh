#!/bin/bash
#
# Configure the KDE Plasma desktop: input devices, app hotkeys, global shortcuts, virtual desktops,
# effects, theme, icons and panels. Run by start_fresh.sh, and safe to re-run on its own.
#
# Usage: configure_kde.sh [WORKSPACES]
#
# WORKSPACES lists extra VS Code workspaces for Meta+<letter>, as "<letter>:<absolute path>,...".
# Exits with status 2 if a setting changed that takes a reboot to apply.
#
# Author: Danylo Malyuta, 2026.

dotfiles=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
workspaces=${1:-}

# --------------------------------------------------------------------------------------------------
# Helpers.
# --------------------------------------------------------------------------------------------------

. "$dotfiles"/scripts/functions.sh

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
# Configuration.
# --------------------------------------------------------------------------------------------------

# Exit with status 2 if these change. Writing an unchanged value leaves the file as is.
kwin_env=~/.config/plasma-workspace/env/kwin-no-vulkan.sh
kde_files=(~/.config/{kcminputrc,kglobalshortcutsrc,kwinrc,ksmserverrc,kdeglobals,plasmashellrc}
	~/.config/plasma-org.kde.plasma.desktop-appletsrc "$kwin_env")
kde_hash() { cat "${kde_files[@]}" 2>/dev/null | md5sum; }
kde_before=$(kde_hash)

# Since Plasma 6.7, KWin's Vulkan path keeps a context on the NVIDIA dGPU, which then never
# suspends. KWin renders with OpenGL on the iGPU anyway. Plasma reads this file at login.
if has_nvidia; then
	mkdir -p "${kwin_env%/*}"
	echo 'export KWIN_DISABLE_VULKAN=1' >"$kwin_env"
else
	rm -f "$kwin_env"
fi

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
# The on-screen keyboard, shown when a text field is tapped in tablet mode. start_fresh.sh installs
# it on the PX13.
plasma_keyboard=/usr/share/applications/org.kde.plasma.keyboard.desktop
[ -f "$plasma_keyboard" ] && kconf --file kwinrc --group Wayland --key InputMethod "$plasma_keyboard"
# It also pops up accented characters when a physical key is held. Holding a key should repeat it.
kconf --file plasmakeyboardrc --group General --key diacriticsPopupEnabled false

# App hotkeys: a KWin script that focuses an app's window on any desktop, or launches the app. It
# launches through kglobalaccel, so each app it launches is registered below, with no key of its
# own.
mkdir -p ~/.local/share/kwin/scripts
ln -sfn "$dotfiles"/config/kwin/apphotkeys ~/.local/share/kwin/scripts/apphotkeys
kconf --file kwinrc --group Plugins --key apphotkeysEnabled true

# VS Code workspaces that Meta+C, Meta+<key> focuses, or opens if no window has them: this repo, and
# those passed in.
declare -A vscode_workspaces=([D]=$dotfiles)
IFS=, read -r -a entries <<<"$workspaces"
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

# Cardwire from the menu shows the running copy instead of starting another, whose tray icon would
# sit next to the first one's. The entry shadows the system one, which it copies.
cardwire_entry=/usr/share/applications/cardwire-gui.desktop
if [ -f "$cardwire_entry" ]; then
	sed "s|^Exec=.*|Exec=\"$dotfiles/bin/cardwire-gui.sh\"|" "$cardwire_entry" \
		>~/.local/share/applications/cardwire-gui.desktop
else
	rm -f ~/.local/share/applications/cardwire-gui.desktop
fi

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
md.obsidian.Obsidian.desktop|Obsidian|_launch|Obsidian|None
org.inkscape.Inkscape.desktop|Inkscape|_launch|Inkscape|None
org.speedcrunch.SpeedCrunch.desktop|SpeedCrunch|_launch|SpeedCrunch|None
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
# Tela-circle icons, in the dark variant to match Breeze Dark. Set after the look and feel, since
# applying one resets the icon theme. plasma-changeicons tells running apps to reload icons, but
# the panel's task manager keeps the icons it already looked up, so restart plasmashell too.
# Fedora doesn't package them, so install them from upstream, to ~/.local/share/icons. The
# installer defaults to the standard folder color, as Tela-circle, Tela-circle-dark and -light.
icon_theme=Tela-circle-dark
if [ ! -d ~/.local/share/icons/"$icon_theme" ]; then
	tela_src=$(mktemp -d)
	git clone -q --depth 1 https://github.com/vinceliuice/Tela-circle-icon-theme.git "$tela_src" &&
		"$tela_src"/install.sh >/dev/null
	rm -rf "$tela_src"
fi
if [ "$(kreadconfig6 --file kdeglobals --group Icons --key Theme)" != "$icon_theme" ]; then
	/usr/libexec/plasma-changeicons "$icon_theme"
	restart_plasmashell=1
fi
# Panels 40 px tall, opaque (0 is adaptive, 1 opaque, 2 translucent), and never floating, even
# with no window maximized.
for id in $(plasma_ids 'panels()'); do
	panel=(plasmashellrc --group PlasmaViews --group "Panel $id")
	plasma_conf "${panel[@]}" --group Defaults --key thickness 40
	plasma_conf "${panel[@]}" --key panelOpacity 1
	plasma_conf "${panel[@]}" --key floating 0
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
	pins=(org.kde.dolphin brave-browser kitty com.microsoft.VSCode md.obsidian.Obsidian)
	[ -f ~/.local/share/applications/pdf-xchange-editor.desktop ] && pins+=(pdf-xchange-editor)
	list=$(printf ',applications:%s.desktop' "${pins[@]}")
	plasma_conf plasma-org.kde.plasma.desktop-appletsrc \
		--group Containments --group "$tm_cont" --group Applets --group "$tm_applet" \
		--group Configuration --group General --key launchers "${list#,}"
fi
# Start menu button icon.
read -r ko_cont ko_applet < <(applet_group org.kde.plasma.kickoff)
if [ -n "${ko_applet:-}" ]; then
	plasma_conf plasma-org.kde.plasma.desktop-appletsrc \
		--group Containments --group "$ko_cont" --group Applets --group "$ko_applet" \
		--group Configuration --group General --key icon "$dotfiles"/config/icons/start_menu.svg
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

[ "$(kde_hash)" = "$kde_before" ] || exit 2
