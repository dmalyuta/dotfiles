# shellcheck shell=bash
# shellcheck disable=SC2034,SC2154 # shares variables with scripts/configure_kde and its other libraries
#
# configure_kde's desktop: virtual desktops, effects, theme and icons, panels, the system tray and
# the start menu.

# Exactly the virtual desktops named by the arguments, in one row. Existing ones are renamed
# rather than recreated, so their windows stay put.
set_virtual_desktops() {
	local names=("$@") ids i id
	vdm() { kwin /VirtualDesktopManager "org.kde.KWin.VirtualDesktopManager.$1" "${@:2}"; }
	# The desktops property is a list of (position, id, name).
	mapfile -t ids < <(dbus_call org.kde.KWin /VirtualDesktopManager \
		org.freedesktop.DBus.Properties.Get org.kde.KWin.VirtualDesktopManager desktops |
		grep -oP "\d+, '\K[^']+")
	for i in "${!names[@]}"; do
		if [ -n "${ids[i]:-}" ]; then
			vdm setDesktopName "${ids[i]}" "${names[i]}"
		else
			vdm createDesktop "$i" "${names[i]}"
		fi
	done
	for id in "${ids[@]:${#names[@]}}"; do
		vdm removeDesktop "$id"
	done
	kwin /VirtualDesktopManager org.freedesktop.DBus.Properties.Set \
		org.kde.KWin.VirtualDesktopManager rows '<uint32 1>'
}

configure_effects() {
	local effect
	# Flash the desktop name for 200 ms on switch. This OSD is a KWin script, which a reconfigure
	# does not load, so start the scripts (apphotkeys too, see set_up_app_hotkeys).
	kconf --file kwinrc --group Plugins --key desktopchangeosdEnabled true
	kconf --file kwinrc --group Script-desktopchangeosd --key PopupHideDelay 200
	kconf --file kwinrc --group Script-desktopchangeosd --key TextOnly true
	kwin /Scripting org.kde.kwin.Scripting.start

	kconf --file ksmserverrc --group General --key loginMode emptySession
	kconf --file ksmserverrc --group General --key confirmLogout false
	kconf --file kdeglobals --group KDE --key AnimationDurationFactor 0
	# Ctrl+Shift+I (see set_global_shortcuts) inverts the screen colors.
	kconf --file kwinrc --group Plugins --key invertEnabled true
	kwin /Effects org.kde.kwin.Effects.loadEffect invert
	# No animations. Also unload them, since a reconfigure leaves running effects loaded.
	for effect in wobblywindows magiclamp translucency squash scale fade glide \
		maximize fullscreen slide fadedesktop; do
		kconf --file kwinrc --group Plugins --key "${effect}Enabled" false
		kwin /Effects org.kde.kwin.Effects.unloadEffect "$effect"
	done
	kwin /KWin org.kde.KWin.reconfigure
}

apply_look_and_feel() {
	[ "$(kreadconfig6 --file kdeglobals --group KDE --key LookAndFeelPackage)" = "$1" ] ||
		plasma-apply-lookandfeel --apply "$1"
}

# Link $2 to $1, a file in the same folder, if $2 doesn't exist.
tela_links=
tela_link() {
	[ -e "$2" ] && return
	ln -s "$1" "$2"
	tela_links=1
	restart_plasmashell=1
}

# Tela-circle icons, from upstream (Fedora doesn't package them).
install_tela_icons() {
	local src icon theme
	if [ ! -d ~/.local/share/icons/"$1" ]; then
		src=$(mktemp -d)
		git clone -q --depth 1 https://github.com/vinceliuice/Tela-circle-icon-theme.git "$src" &&
			"$src"/install.sh >/dev/null
		rm -rf "$src"
	fi
	# The tray asks for battery-090-charging-symbolic and the like, which Tela lacks, so it falls
	# back to battery-symbolic: an empty outline at every charge level. Give Tela's level icons
	# those names.
	for icon in ~/.local/share/icons/Tela-circle*/*/panel/battery-[0-9]*.svg \
		~/.local/share/icons/Tela-circle*/*/*/battery-profile-*.svg; do
		case $icon in *-symbolic.svg) continue ;; esac
		tela_link "$(basename "$icon")" "${icon%.svg}-symbolic.svg"
	done
	# Fully charged on AC with a power profile other than the default, the tray shows
	# battery-profile-<profile>-symbolic instead. Tela has no balanced one, so show the full,
	# plugged-in battery for that.
	for icon in ~/.local/share/icons/Tela-circle*/*/panel/battery-100-charging.svg; do
		tela_link "$(basename "$icon")" "${icon%/*}/battery-profile-balanced-symbolic.svg"
	done
	if [ -n "$tela_links" ]; then
		for theme in ~/.local/share/icons/Tela-circle*/; do gtk-update-icon-cache -qf "$theme"; done
		rm -f ~/.cache/icon-cache.kcache
	fi
}

# The panel keeps the icons it already looked up, so restart plasmashell too.
apply_icon_theme() {
	[ "$(kreadconfig6 --file kdeglobals --group Icons --key Theme)" = "$1" ] && return
	/usr/libexec/plasma-changeicons "$1"
	restart_plasmashell=1
}

# Panels 40 px tall, opaque (0 is adaptive, 1 opaque, 2 translucent), and never floating, even with
# no window maximized.
configure_panels() {
	local id panel
	for id in $(plasma_ids 'panels()'); do
		panel=(plasmashellrc --group PlasmaViews --group "Panel $id")
		plasma_conf "${panel[@]}" --group Defaults --key thickness 40
		plasma_conf "${panel[@]}" --key panelOpacity 1
		plasma_conf "${panel[@]}" --key floating 0
	done
}

# No desktop icons: switch from the Folder View layout to the plain one.
hide_desktop_icons() {
	local id
	for id in $(plasma_ids 'desktops()'); do
		plasma_conf "$applets_rc" --group Containments --group "$id" \
			--key plugin org.kde.desktopcontainment
	done
}

# The PDF editor, which gets pinned and opens PDF files: PDF-XChange if installed, else PdfCraft.
pdf_app=
choose_pdf_app() {
	local app
	for app in pdf-xchange-editor ai.storyteller.pdfcraft; do
		app_installed "$app" && pdf_app=$app && break
	done
	[ -z "$pdf_app" ] || [ "$(xdg-mime query default application/pdf)" = "$pdf_app.desktop" ] ||
		xdg-mime default "$pdf_app.desktop" application/pdf
}

# The task manager's pinned apps: exactly the given ones, in this order. Anything else that was
# pinned is dropped.
pin_apps() {
	local containment applet list
	read -r containment applet < <(applet_group org.kde.plasma.icontasks)
	[ -n "${applet:-}" ] || return 0
	list=$(printf ',applications:%s.desktop' "$@")
	plasma_conf "$applets_rc" --group Containments --group "$containment" --group Applets \
		--group "$applet" --group Configuration --group General --key launchers "${list#,}"
}

# Edit one of the system tray's lists, the key $1 of its config group $tray: append each +item
# that isn't in it, and drop each -item.
tray_list() {
	local key=$1 item list kept=()
	local -A drop=() have=()
	for item in "${@:2}"; do [[ $item == -* ]] && drop[${item:1}]=1; done
	IFS=, read -ra list <<<"$(kreadconfig6 --file "${tray[@]}" --key "$key")"
	for item in "${list[@]}"; do
		[ -n "${drop[$item]:-}" ] || { kept+=("$item"); have[$item]=1; }
	done
	for item in "${@:2}"; do
		[[ $item == +* ]] && [ -z "${have[${item:1}]:-}" ] && kept+=("${item:1}")
	done
	plasma_conf "${tray[@]}" --key "$key" "$(IFS=,; echo "${kept[*]}")"
}

# The system tray: our on-screen keyboard button, the battery always shown on a laptop, and
# brightness, clipboard, Bluetooth and Discover's updates always hidden. The rest show only when
# they have something to show.
configure_system_tray() {
	local containment applet tray
	local buttons=() always_shown=()
	local always_hidden=(org.kde.plasma.brightness org.kde.plasma.clipboard org.kde.plasma.bluetooth
		"Discover Notifier_org.kde.DiscoverNotifier")
	[ -f "$plasma_keyboard" ] && buttons=(dotfiles.oskbutton)
	is_laptop && always_shown=(org.kde.plasma.battery)
	read -r containment applet < <(applet_group org.kde.plasma.systemtray)
	[ -n "${applet:-}" ] || return 0
	tray=("$applets_rc" --group Containments --group "$containment" --group Applets
		--group "$applet" --group General)
	tray_list extraItems "${buttons[@]/#/+}"
	tray_list knownItems "${buttons[@]/#/+}"
	tray_list shownItems "${always_shown[@]/#/+}" "${always_hidden[@]/#/-}"
	tray_list hiddenItems "${always_hidden[@]/#/+}" "${always_shown[@]/#/-}"
}

set_start_menu_icon() {
	local containment applet
	read -r containment applet < <(applet_group org.kde.plasma.kickoff)
	[ -n "${applet:-}" ] || return 0
	plasma_conf "$applets_rc" --group Containments --group "$containment" --group Applets \
		--group "$applet" --group Configuration --group General --key icon "$1"
}

# The start menu's favorites: exactly the given apps, in this order. They're in the activity
# manager's database, so go through its D-Bus API (and read the database only with mode=ro, which
# sees writes still in its WAL).
set_start_menu_favorites() {
	local agent=org.kde.plasma.favorites.applications want order res group app
	fav_link() {
		dbus_call org.kde.ActivityManager /ActivityManager/Resources/Linking \
			"org.kde.ActivityManager.ResourcesLinking.$1" "$agent" "$2" :global >/dev/null
	}
	want=" $(printf 'applications:%s.desktop ' "$@")"
	while read -r res; do
		[ -z "$res" ] || [[ $want == *" $res "* ]] || fav_link UnlinkResourceFromActivity "$res"
	done < <(sqlite3 "file:$HOME/.local/share/kactivitymanagerd/resources/database?mode=ro" \
		"select targettedResource from ResourceLink where initiatingAgent='$agent';" 2>/dev/null)
	# The menu shows them in the order kept for each of its instances, if it has one, else as
	# linked.
	order=$(printf ',%s.desktop' "$@")
	while read -r group; do
		kconf --file kactivitymanagerd-statsrc --group "$group" --key ordering "${order#,}"
	done < <(sed -n 's/^\[\(Favorites-org\.kde\.plasma\.kickoff\.[^]]*\)\]$/\1/p' \
		~/.config/kactivitymanagerd-statsrc 2>/dev/null)
	for app in "$@"; do
		fav_link LinkResourceToActivity "applications:$app.desktop"
	done
}

# Remove every widget of the given types from the panels.
remove_panel_widgets() {
	local types
	types=$(printf '"%s", ' "$@")
	plasma_script "panels().forEach(p => [${types%, }]
		.forEach(type => p.widgets(type).forEach(w => w.remove())))" >/dev/null
}
