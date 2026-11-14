# shellcheck shell=bash
# shellcheck disable=SC2034,SC2154 # shares variables with scripts/configure_kde and its other libraries
#
# configure_kde's shortcuts: the App Hotkeys KWin script (config/kwin/apphotkeys), which focuses an
# app's window on any desktop or launches the app, and the global shortcuts.

# The script launches an app through kglobalaccel, by the "_launch" action of its desktop entry.
# kglobalaccel only keeps an entry across logins if it has a key, or if it's in this folder, which
# it reads at login. So the entries the script launches go here. They aren't in the app menu.
launch_dir=~/.local/share/kglobalaccel

# The VS Code workspaces that Meta+C, Meta+<letter> focuses, or opens if no window has them: this
# repo, and those answered in start_fresh as "<letter>:<absolute path>,...".
declare -A vscode_workspaces=([D]=$dotfiles)
read_workspaces() {
	local entry entries
	IFS=, read -r -a entries <<<"$(answer workspaces)"
	for entry in "${entries[@]}"; do
		vscode_workspaces[${entry%%:*}]=${entry#*:}
	done
}

# The letters of the workspaces that have a shortcut or a desktop entry now.
known_workspace_letters() {
	{
		shortcut_names kwin | grep -oP "'App Hotkeys: VS Code workspace \K[A-Z]"
		kglobalaccel allComponents | grep -oP 'vscode_workspace_\K[a-z](?=_desktop)'
		printf '%s\n' "$launch_dir"/vscode-workspace-?.desktop |
			grep -oP 'vscode-workspace-\K[a-z](?=\.desktop$)'
	} | tr '[:lower:]' '[:upper:]' | sort -u
}

# Drop the shortcuts and desktop entry of each workspace no longer in the list, which frees their
# keys. Only those: re-registering a shortcut right after unregistering it is unreliable.
drop_removed_workspaces() {
	local key
	while read -r key; do
		[ -n "${vscode_workspaces[$key]:-}" ] && continue
		kglobalaccel unregister kwin "App Hotkeys: VS Code workspace $key" >/dev/null
		kglobalaccel unregister "vscode-workspace-${key,,}.desktop" _launch >/dev/null
		rm -f "$launch_dir"/vscode-workspace-"${key,,}".desktop
	done < <(known_workspace_letters)
	# Earlier versions put the entries in the app menu's folder, hidden, where kglobalaccel forgot
	# them at logout.
	rm -f ~/.local/share/applications/vscode-workspace-?.desktop
}

# A desktop entry per workspace, named by its letter, for the script to launch it by. And the list
# itself, which the script reads from its config when it loads.
write_workspace_launchers() {
	local key dir config=
	for key in "${!vscode_workspaces[@]}"; do
		dir=${vscode_workspaces[$key]}
		cat >"$launch_dir"/vscode-workspace-"${key,,}".desktop <<EOF
[Desktop Entry]
Type=Application
Name=VS Code: ${dir##*/}
Exec=code "$dir"
Icon=vscode
EOF
		config+="$key=$dir;"
	done
	kconf --file kwinrc --group Script-apphotkeys --key vscodeWorkspaces "$config"
}

set_up_app_hotkeys() {
	mkdir -p ~/.local/share/kwin/scripts "$launch_dir"
	ln -sfn "$dotfiles"/config/kwin/apphotkeys ~/.local/share/kwin/scripts/apphotkeys
	kconf --file kwinrc --group Plugins --key apphotkeysEnabled true
	# Unloaded, so that Scripting.start (in configure_effects) loads it again with the new list.
	kwin /Scripting org.kde.kwin.Scripting.unloadScript apphotkeys
	read_workspaces
	drop_removed_workspaces
	write_workspace_launchers
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
			failed "Unknown key '$part' in the shortcut '$1'"
			return 1
		fi
	done
	echo "$code"
}

# The actions bound to the key with Qt key code $1, one "action|its name|component|its name|keys"
# per line, from kglobalaccel's list of tuples.
actions_with_key() {
	kglobalaccel globalShortcutsByKey "([$1],)" "(0,)" |
		grep -oP "\('[^']*', '[^']*', '[^']*', '[^']*', '[^']*', '[^']*', \[[^]]*\]" |
		sed -E "s/^\('([^']*)', '([^']*)', '([^']*)', '([^']*)', '[^']*', '[^']*', \[(.*)\]$/\1|\2|\3|\4|\5/"
}

# Remove the key with Qt key code $1 from the actions that have it, except action $3 of component
# $2, keeping their other keys. kglobalaccel does not let two actions share a key.
take_key() {
	local action action_name component component_name keys
	while IFS='|' read -r action action_name component component_name keys; do
		[ "$component|$action" = "$2|$3" ] && continue
		keys=$(tr -d ' ' <<<"$keys" | tr , '\n' | grep -vx "$1" | paste -sd ,)
		kglobalaccel setForeignShortcut \
			"['$component','$action','$component_name','$action_name']" "[${keys:-0}]" >/dev/null
	done < <(actions_with_key "$1")
}

# Bind a global shortcut, taking the key from any other action that has it. Args: component, its
# friendly name, action, its friendly name, keys (see qt_keycode, or None to unbind).
set_shortcut() {
	local id code
	# Skip on error: an empty key list would unbind the action.
	code=$(qt_keycode "$5") || return
	id="['$1','$3','$2','$4']"
	[ "$code" -ne 0 ] && take_key "$code" "$1" "$3"
	# kglobalaccel ignores a desktop entry's shortcut until it notices the entry, which for a new one
	# takes a few seconds after kbuildsycoca6, so retry while it is missing.
	for _ in {1..20}; do
		kglobalaccel doRegister "$id" >/dev/null
		kglobalaccel setForeignShortcut "$id" "[$code]" >/dev/null
		[[ $1 == *.desktop ]] && [ -f ~/.local/share/applications/"$1" ] || return 0
		shortcut_names "$1" 2>/dev/null | grep -qF "'$3'" && return
		sleep 0.5
	done
	failed "Could not register the $2 shortcut."
}

# Link an app's desktop entry into $launch_dir, so its launch action stays across logins.
keep_launch_entry() {
	local d
	for d in ~/.local/share/applications /usr/share/applications \
		/var/lib/flatpak/exports/share/applications; do
		[ -f "$d/$1" ] && ln -sfn "$d/$1" "$launch_dir/$1" && return
	done
}

# Set through kglobalaccel's D-Bus API, because it keeps the shortcuts in memory and overwrites
# edits to kglobalshortcutsrc. An app is bound by its desktop entry, whose "_launch" action runs
# its Exec. Each key is taken from KDE's default action for it, if any, e.g. Meta+Up from Quick
# Tile Top.
set_global_shortcuts() {
	local shortcut key name
	# Rebuild the cache that kglobalaccel finds the desktop entries in.
	kbuildsycoca6 >/dev/null 2>&1
	find "$launch_dir" -xtype l -delete
	# Component, its name, action, its name, keys.
	while IFS='|' read -r -a shortcut; do
		set_shortcut "${shortcut[@]}"
		[ "${shortcut[2]}" = _launch ] && keep_launch_entry "${shortcut[0]}"
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
}
