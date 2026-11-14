# shellcheck shell=bash
# shellcheck disable=SC2034,SC2154 # shares variables with scripts/configure_kde and its other libraries
#
# configure_kde's helpers: talking to KWin, Plasma and the shortcuts service over D-Bus, writing
# settings, and noticing which ones changed.

dbus_call() {
	local dest=$1 path=$2 method=$3
	shift 3
	gdbus call --session --dest "$dest" --object-path "$path" --method "$method" "$@"
}

kwin() { dbus_call org.kde.KWin "$@" >/dev/null; }

# Call a method of kglobalaccel, which holds the global shortcuts.
kglobalaccel() { dbus_call org.kde.kglobalaccel /kglobalaccel "org.kde.KGlobalAccel.$1" "${@:2}"; }

# The names of a kglobalaccel component's shortcuts. Its D-Bus path is its name with
# non-alphanumerics turned into "_".
shortcut_names() {
	dbus_call org.kde.kglobalaccel "/component/${1//[^A-Za-z0-9]/_}" \
		org.kde.kglobalaccel.Component.shortcutNames
}

# Run JavaScript in plasmashell, see Plasma's desktop scripting API.
plasma_script() {
	dbus_call org.kde.plasmashell /PlasmaShell org.kde.PlasmaShell.evaluateScript "$1"
}

# Ids of the given plasma objects. gdbus prints the returned string as ('...',).
plasma_ids() {
	plasma_script "print($1.map(x => x.id).join(' '))" | sed -E "s/^\('(.*)',\)$/\1/"
}

applets_rc=plasma-org.kde.plasma.desktop-appletsrc
kwin_env=~/.config/plasma-workspace/env/kwin-no-vulkan.sh
# A change to any of these needs a reboot (or a new login) to apply everywhere.
kde_files=(~/.config/{kcminputrc,kglobalshortcutsrc,kwinrc,ksmserverrc,kdeglobals,plasmashellrc}
	~/.config/"$applets_rc" "$kwin_env")
kde_before=$(mktemp -d)
trap 'rm -rf "$kde_before"' EXIT

# Keep a copy of each of those files, to compare with at the end. Writing an unchanged value
# leaves a file as it is.
remember_settings() {
	local f
	for f in "${kde_files[@]}"; do
		[ -f "$f" ] && cp "$f" "$kde_before/${f##*/}"
	done
}

# Ask for a reboot for each file that changed, naming up to 5 of the keys that changed in it.
report_changed_settings() {
	local f old keys n
	for f in "${kde_files[@]}"; do
		old=$kde_before/${f##*/}
		{ [ -e "$old" ] || [ -e "$f" ]; } || continue
		cmp -s "$old" "$f" 2>/dev/null && continue
		keys=$(diff "$old" "$f" 2>/dev/null | sed -n 's/^[<>] \([^=]*\)=.*/\1/p' | sort -u)
		n=$(wc -l <<<"$keys")
		[ -n "$keys" ] &&
			keys=": $(awk 'NR <= 5 { printf "%s%s", (NR > 1 ? ", " : ""), $0 }' <<<"$keys")" &&
			[ "$n" -gt 5 ] && keys+=" and $((n - 5)) more"
		reboot_for "KDE setting changed in ${f##*/}$keys"
	done
}

# Plasmashell only reads its settings on start, and writes its in-memory copy back over the file,
# so set them in the file and restart it at the end if any changed. Args: file, kreadconfig6's
# --group and --key options, value.
restart_plasmashell=
plasma_conf() {
	local file=$1 value=${!#} args=("${@:2:$#-2}")
	[ "$(kreadconfig6 --file "$file" "${args[@]}")" = "$value" ] && return
	kconf --file "$file" "${args[@]}" "$value"
	restart_plasmashell=1
}

# The config group of the panel applet with the given plugin, printed as "containment applet".
applet_group() {
	awk -F'[][]' -v want="plugin=$1" '
		/^\[Containments]\[[0-9]+]\[Applets]\[[0-9]+]$/ { c = $4; a = $8 }
		$0 == want { print c, a; exit }
	' ~/.config/"$applets_rc"
}

# Whether the app with this desktop file id (without .desktop) is installed.
app_installed() {
	[ -f ~/.local/share/applications/"$1".desktop ] || [ -f /usr/share/applications/"$1".desktop ]
}

# Show the system's desktop entry $1 changed by sed script $2, through a copy in the user's folder,
# which shadows it. Without the app, drop the copy.
shadow_desktop_entry() {
	local system=/usr/share/applications/$1 mine=~/.local/share/applications/$1
	if [ -f "$system" ]; then
		sed "$2" "$system" >"$mine"
	else
		rm -f "$mine"
	fi
}
