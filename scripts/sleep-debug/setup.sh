#!/bin/bash
# Turn the laptop's sleep and hibernation debug log on or off: the sleep-debug hook and its boot
# service, and the hibernate-trace initramfs module, all in this folder (see sleep-debug for what it
# logs and how to read it). Run by start_fresh.sh, which asks; safe to re-run.
# Usage: setup.sh on|off
# "off" removes all of it but keeps the logs in /var/log/sleep-debug.
# Exits with status 2 if the initramfs changed, which takes a reboot to apply.
# Author: Danylo Malyuta, 2026.

dotfiles=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)
. "$dotfiles"/scripts/functions.sh

module=/usr/lib/dracut/modules.d/90hibernate-trace
conf=/etc/dracut.conf.d/91-sleep-debug.conf
hook=/usr/lib/systemd/system-sleep/sleep-debug
unit=/etc/systemd/system/sleep-debug.service
var=/sys/firmware/efi/efivars/HibernateTrace-8bc5b097-6e23-408e-9a7d-2bad8e8739d1

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
	install_script_file sleep-debug/sleep-debug.service "$unit" && sudo systemctl daemon-reload
	sudo systemctl enable sleep-debug
	;;
off)
	for f in "$module" "$conf" "$hook" "$unit" "$var"; do
		[ -e "$f" ] || continue
		say "Removing the sleep debug log, keeping its logs in /var/log/sleep-debug."
		sudo systemctl disable sleep-debug 2>/dev/null
		sudo chattr -i "$var" 2>/dev/null
		sudo rm -rf "$module" "$conf" "$hook" "$unit" "$var" /run/sleep-debug*
		sudo systemctl daemon-reload
		# The hook's kernel settings, normally reset by the next boot.
		echo 0 | sudo tee /sys/power/pm_trace /sys/power/pm_debug_messages >/dev/null
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
