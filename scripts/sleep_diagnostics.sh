#!/bin/bash
#
# Turn the noisy suspend/resume diagnostics on or off.
#
#     sleep_diagnostics.sh on       collect everything, for chasing a bug
#     sleep_diagnostics.sh off      back to normal
#     sleep_diagnostics.sh status   what is active now, and after a reboot
#
# What this toggles:
#
# The tmpfiles rule it installs is devices/pm-debug.conf.
#
#     pm_print_times          per-device suspend/resume timings. ~740 lines a
#                             cycle, and the only way to see which device is
#                             slow (it is the HP dock: ~2.7s of the ~4.7s).
#     pm_debug_messages       extra detail from the PM core.
#     no_console_suspend      keeps the console alive across the transition, so
#                             a hang has somewhere to print. Needs a reboot.
#     log_buf_len=8M          the above can overflow the default ring buffer
#                             before journald gets to read it. Needs a reboot.
#
# What this does NOT touch, because it is how the machine is meant to be set up
# and start_fresh.sh installs it:
#
#     fbcon=nodefer                              the resume fix itself
#     NVreg_PreserveVideoMemoryAllocations       video memory across suspend
#     /usr/lib/systemd/system-sleep/usb-wakeup   Keychron wakes, Razer does not
#     /usr/lib/systemd/system-sleep/zz-sleep-forensics
#     journald SyncIntervalSec=10s               logs survive a power-cycle
#     kernel.sysrq=1                             Alt+SysRq+W/L/S
#     sshd (only when ufw is enabled)            a way in when the screen is dead
#
# Author: Danylo Malyuta, 2026.

set -euo pipefail

# Resolved from the script's own location, through any symlink, so it works the
# same whether it is run by path or from $PATH.
script_dir=$(cd "$(dirname "$(readlink -f "${BASH_SOURCE[0]}")")" 2>/dev/null && pwd)
[ -n "$script_dir" ] || script_dir=$PWD
: "${DEVICES_DIR:=$(dirname "$script_dir")/devices}"

die() {
	printf '\033[1;31m!!\033[0m %s\n' "$*" >&2
	exit 1
}

grub=/etc/default/grub
key=GRUB_CMDLINE_LINUX_DEFAULT
boot_opts="no_console_suspend log_buf_len=8M"
tmpfiles=/etc/tmpfiles.d/pm-debug.conf
knobs="/sys/power/pm_print_times /sys/power/pm_debug_messages"

# The options on the line, however it is quoted; see set_boot_opts below.
grub_cmdline() { sed -n "s/^${key}=[\"']\(.*\)[\"']$/\1/p" "$grub"; }

# $1 is "add" or "drop". Returns 0 only when the file actually changed, so a
# repeat run does not re-run update-grub.
set_boot_opts() {
	local mode=$1 cur new opt quote
	# Refuse to touch a line that cannot be parsed back exactly: rewriting it
	# would silently drop every option it carries. A fresh install writes the
	# line either as KEY="..." or as KEY='...' -- both are quoted the same way
	# by the shell that sources this file, and which one it is differs between
	# Ubuntu releases and flavours. Anything else is somebody's hand edit.
	# The quote the file already uses is the one it gets written back with, so
	# the only change to the line is the option list itself.
	case "$(grep -c "^${key}=" "$grub" || true)" in
	0) quote='"' ;;
	1)
		if grep -q "^${key}=\"[^\"]*\"$" "$grub"; then
			quote='"'
		elif grep -q "^${key}='[^']*'$" "$grub"; then
			quote="'"
		else
			die "$grub: cannot parse the $key line, leaving it alone."
		fi
		;;
	*) die "$grub: more than one $key line, leaving them alone." ;;
	esac
	cur=$(grub_cmdline)
	# Pad so every option is space-delimited and a match cannot catch the
	# substring of a longer one.
	new=" $cur "
	for opt in $boot_opts; do
		new=${new// $opt / }
		if [ "$mode" = add ]; then
			new="$new$opt "
		fi
	done
	new=$(printf '%s' "$new" | tr -s ' ' | sed 's/^ //; s/ $//')
	if [ "$new" = "$cur" ]; then
		return 1
	fi
	sudo cp -a "$grub" "$grub.bak-$(date +%Y%m%d-%H%M%S)"
	sudo sed -i "s|^${key}=.*|${key}=${quote}${new}${quote}|" "$grub"
}

set_knobs() {
	local value=$1 f
	for f in $knobs; do
		if [ -e "$f" ]; then
			echo "$value" | sudo tee "$f" >/dev/null
		fi
	done
}

case "${1:-status}" in
on)
	[ -f "$DEVICES_DIR/pm-debug.conf" ] || die "No $DEVICES_DIR/pm-debug.conf.
   It lives in devices/ at the top of the repo, next to start_fresh.sh. Point
   DEVICES_DIR at it if the repo is somewhere unusual."
	sudo install -m 644 "$DEVICES_DIR/pm-debug.conf" "$tmpfiles"
	set_knobs 1
	echo "== pm_print_times and pm_debug_messages on, now and at every boot."
	if set_boot_opts add; then
		echo "== Kernel command line: $(grub_cmdline)"
		sudo update-grub
		echo
		echo "Reboot to pick up $boot_opts."
	else
		echo "== Kernel command line already carries $boot_opts."
	fi
	;;
off)
	sudo rm -f "$tmpfiles"
	set_knobs 0
	echo "== pm_print_times and pm_debug_messages off."
	if set_boot_opts drop; then
		echo "== Kernel command line: $(grub_cmdline)"
		sudo update-grub
		echo
		echo "Reboot to drop $boot_opts."
	else
		echo "== Kernel command line already clean."
	fi
	;;
status)
	for f in $knobs; do
		printf '%-24s %s\n' "$(basename "$f")" "$(cat "$f" 2>/dev/null || echo '(unreadable, try sudo)')"
	done
	printf '%-24s %s\n' "tmpfiles rule" \
		"$([ -f "$tmpfiles" ] && echo present || echo absent)"
	echo
	for opt in $boot_opts; do
		printf '%-24s running=%-4s next boot=%s\n' "$opt" \
			"$(grep -qw -- "$opt" /proc/cmdline && echo yes || echo no)" \
			"$(grep -q -- "$opt" "$grub" && echo yes || echo no)"
	done
	echo
	printf '%-24s running=%-4s next boot=%s\n' "fbcon=nodefer (the fix)" \
		"$(grep -qw -- fbcon=nodefer /proc/cmdline && echo yes || echo no)" \
		"$(grep -q -- fbcon=nodefer "$grub" && echo yes || echo no)"
	;;
*)
	echo "usage: $(basename "$0") on|off|status" >&2
	exit 2
	;;
esac
