# shellcheck shell=bash
# shellcheck disable=SC2034,SC2154 # shares variables with scripts/configure_hardware and its other libraries
#
# configure_hardware's setup of the one ASUS laptop model that is_asus_laptop knows: keyboard
# backlight, power profile and limits, tablet mode.

# Keep the keyboard backlight dark from the initramfs on (scripts/dracut/asus-kbd-dark), as
# asus-kbd-dark below does later in the boot: the controller forgets it at each power-off, so
# without this the backlight pulses for the first seconds of a cold boot, and after a hibernation
# until its image is loaded. Its program is built here, and kept with its source to rebuild only
# when that changes.
set_up_asus_keyboard_backlight_initramfs() {
	local src=scripts/dracut/asus-kbd-dark
	local module=/usr/lib/dracut/modules.d/90asus-kbd-dark
	local program=/usr/local/lib/asus-kbd-dark/asus-kbd-dark-initrd
	local build f rebuild=
	if [ ! -f "$program" ] || ! sudo cmp -s "$dotfiles/$src/${program##*/}.c" "$program.c"; then
		build=$(mktemp -d)
		if gcc -O2 -Wall -Wextra -o "$build/program" "$dotfiles/$src/${program##*/}.c"; then
			sudo install -Z -Dm755 "$build/program" "$program"
			sudo install -Z -Dm644 "$dotfiles/$src/${program##*/}.c" "$program.c"
		else
			failed "Could not build the keyboard backlight's initramfs step, so it will pulse" \
				"at power-on."
		fi
		rm -rf "$build"
	fi
	[ -f "$program" ] || return 0
	install_script_file "${src#scripts/}/module-setup.sh" "$module/module-setup.sh" 755 && rebuild=1
	f=asus-kbd-dark-initrd.service
	install_script_file "${src#scripts/}/$f" "$module/$f" && rebuild=1
	write_root_file /etc/dracut.conf.d/92-asus-kbd-dark.conf <<'EOF' && rebuild=1
# The ASUS keyboard backlight stays dark from the initramfs on, see scripts/configure_hardware.
add_dracutmodules+=" asus-kbd-dark "
EOF
	initramfs_has usr/bin/asus-kbd-dark-initrd "$program" || rebuild=1
	if [ -n "$rebuild" ]; then
		sudo dracut -f --regenerate-all
		reboot_for "initramfs rebuilt for the keyboard backlight"
	fi
}

# Keep the keyboard backlight dark while the laptop starts, instead of pulsing until asusd is up
# (scripts/bin/asus-kbd-dark), and reset the keyboard controller after a hibernation
# (scripts/bin/asus-kbd-rebind).
set_up_asus_keyboard_backlight() {
	local led=/sys/class/leds/asus::kbd_backlight
	# Run once now as well, at the brightness it has: the controller keeps "no animation at
	# power-on" from it.
	install_command asus-kbd-dark &&
		sudo /usr/local/bin/asus-kbd-dark "$(cat "$led/brightness" 2>/dev/null || echo 0)"
	install_udev_rule 90-asus-kbd-dark.rules
	set_up_asus_keyboard_backlight_initramfs
	if installed asusctl; then
		# asusd sets the backlight, while systemd-backlight restores whatever it saved at
		# shutdown, which can be 0.
		sudo systemctl mask "systemd-backlight@leds:${led##*/}.service"
		install_command asus-kbd-rebind
		install_unit asus-kbd-rebind.service
		sudo systemctl enable asus-kbd-rebind
	fi
}

# Keep the power profile that was last chosen (in KDE's battery menu, or with powerprofilesctl):
# neither KDE nor asusd switches it on AC or on battery. tuned-ppd holds it, sets platform_profile
# from it (which asusd follows), and saves it in /etc/tuned/ppd_base_profile, from which it starts
# at the next boot.
keep_chosen_power_profile() {
	local p source
	# Without the key, KDE leaves the profile unchanged for that power source.
	for p in AC Battery LowBattery; do
		kconf --file powerdevilrc --group "$p" --group Performance --key PowerProfile --delete
	done
	busctl --user call org.kde.Solid.PowerManagement /org/kde/Solid/PowerManagement \
		org.kde.Solid.PowerManagement reparseConfiguration >/dev/null 2>&1
	systemctl -q is-active asusd || return 0
	for source in Ac Battery; do
		busctl --system set-property xyz.ljones.Asusd /xyz/ljones xyz.ljones.Platform \
			"ChangePlatformProfileOn$source" b false
	done
}

# Per-profile CPU power limits, lower in Quiet for battery life, see scripts/bin/asus-power-limits.
# With Secure Boot on, acpi_call only loads once its signing key is enrolled, see start_fresh.
install_power_limits() {
	install_sleep_hook bin/asus-power-limits
	install_unit asus-power-limits.service
	install_command asus-power-limits && sudo systemctl try-restart asus-power-limits
	sudo systemctl enable --now asus-power-limits
}

# The laptop is a 2-in-1, but asus-nb-wmi has no quirk for it, so it never reports tablet mode. Use
# its lid-flip sensor, on "Asus WMI hotkeys", which keyd leaves alone.
enable_tablet_mode_switch() {
	write_root_file /etc/modprobe.d/asus-tablet-mode.conf <<'EOF' &&
options asus_nb_wmi tablet_mode_sw=2
EOF
		reboot_for "tablet mode switch (/etc/modprobe.d/asus-tablet-mode.conf)"
}
