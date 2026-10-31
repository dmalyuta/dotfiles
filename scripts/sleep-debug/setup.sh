#!/bin/bash
# Turn the laptop's sleep and hibernation debug log on or off: the sleep-debug hook, its boot and
# wake services, and the hibernate-trace initramfs module, all in this folder (see sleep-debug for
# what it logs and how to read it). Also:
# - Hibernate on AC too, so that a hibernation that hangs at power-off doesn't drain the battery
#   flat, which also resets the RTC and loses the pm_trace in it.
# - Let a crashing kernel save its log to EFI variables (efi-pstore, off by default on Fedora), for
#   the hibernation watchdog: a kernel command line option, which takes a reboot.
# - Set the chipset's watchdog (sp5100_tco), the hibernation's backstop, to reset after 10 min,
#   after which the initramfs powers off (hibernate-trace/): only after a reset can it tell that the
#   watchdog fired, which a power-off clears (2026-10-30 tests). Only the hook arms it, during a
#   hibernation; otherwise only systemd uses it, to bound a hung reboot (RebootWatchdogSec), which
#   then also ends powered off.
# - Show what the hook finds wrong in a window in the desktop session (sleep-debug-alert, a user
#   service for every user).
# Run by start_fresh.sh, which asks; safe to re-run.
# Usage: setup.sh on|off
# "off" removes all of it but keeps the logs in /var/log/sleep-debug.
# Exits with status 2 if the initramfs or the kernel command line changed, which take a reboot.
# Author: Danylo Malyuta, 2026.

dotfiles=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)
. "$dotfiles"/scripts/functions.sh

module=/usr/lib/dracut/modules.d/90hibernate-trace
conf=/etc/dracut.conf.d/91-sleep-debug.conf
hook=/usr/lib/systemd/system-sleep/sleep-debug
unit=/etc/systemd/system/sleep-debug.service
wake_unit=/etc/systemd/system/sleep-debug-wake.service
alert_bin=/usr/local/bin/sleep-debug-alert
alert_units="sleep-debug-alert.service sleep-debug-alert.path"
efi=/sys/firmware/efi/efivars
var=$efi/HibernateTrace-8bc5b097-6e23-408e-9a7d-2bad8e8739d1
crash_off=$efi/HibernateCrashOff-8bc5b097-6e23-408e-9a7d-2bad8e8739d1
tco_fired=$efi/HibernateTcoFired-8bc5b097-6e23-408e-9a7d-2bad8e8739d1
# After 50-suspend-then-hibernate.conf from scripts/configure_hardware.sh, so it wins.
sleep_conf=/etc/systemd/sleep.conf.d/60-sleep-debug.conf
# How much of the kernel's log a crash saves, counted compressed: 64 KB took 114 EFI variables of
# ~1 KB for ~190 KB of log (a test crash, 2026-10-30), nearly all of the 128 KB of variables that
# had 89 KB free, which some firmware takes badly. 32 KB is ~95 KB of log, ~15 min of kernel
# messages: enough to reach back from the watchdog's crash to why the power-off failed, 5 min
# before.
kmsg_bytes=32768
pstore_args="efi_pstore.pstore_disable=0 pstore.kmsg_bytes=$kmsg_bytes"
tco_conf=/etc/modprobe.d/sleep-debug-tco.conf

case ${1:-} in
on)
	is_laptop || { echo "The sleep debug log is only for the laptop." >&2 && exit 1; }
	write_root_file "$conf" <<'EOF' && rebuild=1
# Debug log of hibernation, see scripts/sleep-debug in the dotfiles.
add_dracutmodules+=" hibernate-trace "
EOF
	for f in module-setup.sh hibernate-trace-save hibernate-trace-save.service; do
		mode=0755
		[ "$f" = hibernate-trace-save.service ] && mode=0644
		install_script_file "sleep-debug/hibernate-trace/$f" "$module/$f" "$mode" && rebuild=1
	done
	install_script_file sleep-debug/sleep-debug "$hook" 0755
	install_script_file sleep-debug/sleep-debug.service "$unit" && reload=1
	install_script_file sleep-debug/sleep-debug-wake.service "$wake_unit" && reload=1
	[ -n "${reload:-}" ] && sudo systemctl daemon-reload
	sudo systemctl enable sleep-debug sleep-debug-wake
	install_script_file sleep-debug/sleep-debug-alert "$alert_bin" 0755
	for u in $alert_units; do
		install_script_file "sleep-debug/$u" "/etc/systemd/user/$u"
	done
	sudo mkdir -p /var/log/sleep-debug/alerts
	# shellcheck disable=SC2086
	sudo systemctl --global enable $alert_units
	# And in this session, without logging in again.
	if systemctl --user -q is-active graphical-session.target 2>/dev/null; then
		systemctl --user daemon-reload
		# shellcheck disable=SC2086
		systemctl --user start $alert_units
	fi
	write_root_file "$sleep_conf" <<'EOF'
# Debug log of hibernation, see scripts/sleep-debug in the dotfiles: sleep then hibernate on AC too.
[Sleep]
HibernateOnACPower=yes
EOF
	# grubby keeps an option once (updating its value), and new kernels take the running one's
	# command line. The log's size also applies now, through pstorefs.
	sudo grubby --update-kernel=ALL --args="$pstore_args"
	findmnt -n /sys/fs/pstore >/dev/null &&
		sudo mount -o remount,kmsg_bytes="$kmsg_bytes" /sys/fs/pstore
	# The driver reads these when it loads, from the initramfs at boot (so rebuild it), and nothing
	# holds it open between reboots (so load it again now).
	if write_root_file "$tco_conf" <<'EOF'; then
# Hibernation watchdog backstop, see scripts/sleep-debug in the dotfiles: reset after 10 min (and
# the initramfs then powers off).
options sp5100_tco action=0 heartbeat=600
EOF
		sudo modprobe -r sp5100_tco && sudo modprobe sp5100_tco
	fi
	# Also rebuild if an initramfs lacks the current module or those settings, as when a rebuild was
	# interrupted or the settings came after it.
	for img in /boot/initramfs-*.img; do
		[[ $img == *rescue* ]] && continue
		sudo lsinitrd -f usr/bin/hibernate-trace-save "$img" 2>/dev/null |
			cmp -s - "$dotfiles"/scripts/sleep-debug/hibernate-trace/hibernate-trace-save || rebuild=1
		sudo lsinitrd -f "${tco_conf#/}" "$img" 2>/dev/null | cmp -s - "$tco_conf" || rebuild=1
	done
	if ! grep -q 'efi_pstore.pstore_disable=0' /proc/cmdline; then
		say "Reboot for the hibernation watchdog: until then a hibernation runs without it."
		reboot=1
	fi
	;;
off)
	for f in "$module" "$conf" "$hook" "$unit" "$wake_unit" "$alert_bin" "$sleep_conf" "$tco_conf" \
		"$var" "$crash_off" "$tco_fired"; do
		[ -e "$f" ] || continue
		say "Removing the sleep debug log, keeping its logs in /var/log/sleep-debug."
		sudo systemctl disable sleep-debug sleep-debug-wake 2>/dev/null
		# shellcheck disable=SC2086
		sudo systemctl --global disable $alert_units 2>/dev/null
		# shellcheck disable=SC2086
		systemctl --user stop $alert_units 2>/dev/null
		sudo chattr -i "$var" "$crash_off" "$tco_fired" 2>/dev/null
		sudo rm -rf "$module" "$conf" "$hook" "$unit" "$wake_unit" "$alert_bin" "$sleep_conf" \
			"$tco_conf" "$var" "$crash_off" "$tco_fired" /run/sleep-debug*
		for u in $alert_units; do sudo rm -f "/etc/systemd/user/$u"; done
		sudo systemctl daemon-reload
		systemctl --user daemon-reload 2>/dev/null
		# Also the size an earlier version set.
		sudo grubby --update-kernel=ALL \
			--remove-args="$pstore_args pstore.kmsg_bytes=65536 pstore.kmsg_bytes"
		sudo modprobe -r sp5100_tco && sudo modprobe sp5100_tco
		# The hook's kernel settings, normally reset by the next boot.
		echo 0 | sudo tee /sys/power/pm_trace /sys/power/pm_debug_messages >/dev/null
		sudo modprobe -r softdog 2>/dev/null
		rebuild=1
		break
	done
	;;
*)
	echo "Usage: $0 on|off" >&2
	exit 1
	;;
esac

if [ -n "${rebuild:-}" ]; then
	sudo dracut -f --regenerate-all
	exit 2
fi
[ -z "${reboot:-}" ] || exit 2
