# shellcheck shell=bash
# shellcheck disable=SC2034,SC2154 # shares variables with scripts/configure_kde and its other libraries
#
# configure_kde's input devices: keyboard, cursor, pointer devices and the on-screen keyboard.

# Since Plasma 6.7, KWin's Vulkan path keeps a context on the NVIDIA dGPU, which then never
# suspends. KWin renders with OpenGL on the iGPU anyway. Plasma reads this file at login.
keep_kwin_off_vulkan() {
	if has_nvidia; then
		mkdir -p "${kwin_env%/*}"
		echo 'export KWIN_DISABLE_VULKAN=1' >"$kwin_env"
	else
		rm -f "$kwin_env"
	fi
}

configure_keyboard_and_cursor() {
	kconf --file kcminputrc --group Keyboard --key RepeatDelay 150
	kconf --file kcminputrc --group Keyboard --key RepeatRate 50
	kconf --file kcminputrc --group Mouse --key cursorSize 24
	# Tell running apps the cursor changed (5 is KGlobalSettings::CursorChanged).
	gdbus emit --session --object-path /KGlobalSettings \
		--signal org.kde.KGlobalSettings.notifyChange 5 0
}

# The pointer devices, one "vendor|product|name" per line, with the ids in decimal as KDE's
# settings name them.
pointer_devices() {
	gawk -v RS= '/Handlers=[^\n]*mouse/ &&
		match($0, /Vendor=(\w+) Product=(\w+).*N: Name="([^"]*)"/, m) {
			print strtonum("0x" m[1]) "|" strtonum("0x" m[2]) "|" m[3]
		}' /proc/bus/input/devices
}

# Libinput settings are per device: touchpads scroll the traditional way, mice are slowed down.
configure_pointer_devices() {
	local vendor product name device
	while IFS='|' read -r vendor product name; do
		device=(--file kcminputrc --group Libinput --group "$vendor" --group "$product" --group "$name")
		if [[ ${name,,} == *touchpad* ]]; then
			kconf "${device[@]}" --key NaturalScroll false
		else
			kconf "${device[@]}" --key PointerAcceleration -- -0.325
		fi
	done < <(pointer_devices)
}

# The on-screen keyboard, on a machine that folds into a tablet. KWin would pop it up over the app
# whenever a text field is tapped, so it never shows by itself (VirtualKeyboardMode 0): a tray
# button, shown in tablet mode, brings it up, see config/plasma/oskbutton.
plasma_keyboard=/usr/share/applications/org.kde.plasma.keyboard.desktop
set_up_onscreen_keyboard() {
	if [ -f "$plasma_keyboard" ]; then
		kconf --file kwinrc --group Wayland --key InputMethod "$plasma_keyboard"
		kconf --file kwinrc --group Wayland --key VirtualKeyboardMode 0
		# KWin reads the mode at login, so also set it live.
		kwin /VirtualKeyboard org.freedesktop.DBus.Properties.Set org.kde.kwin.VirtualKeyboard mode '<0>'
		mkdir -p ~/.local/share/plasma/plasmoids
		ln -sfn "$dotfiles"/config/plasma/oskbutton ~/.local/share/plasma/plasmoids/dotfiles.oskbutton
	fi
	# It also pops up accented characters when a physical key is held. Holding a key should repeat
	# it.
	kconf --file plasmakeyboardrc --group General --key diacriticsPopupEnabled false
}
