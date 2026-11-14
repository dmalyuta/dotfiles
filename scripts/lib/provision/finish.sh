# shellcheck shell=bash
# shellcheck disable=SC2034,SC2154 # shares variables with provision and its other libraries
#
# The end of a provision run: the Fedora upgrade, what failed, and the reboot.

# Download the Fedora upgrade chosen in ask_fedora_upgrade. The reboot at the end installs it.
download_fedora_upgrade() {
	if sudo dnf system-upgrade download -y --releasever="$upgrade_to"; then
		fedora_upgrade=1
		reboot_for "upgrade to Fedora $upgrade_to, which installs while it reboots"
	else
		failed "Could not download the Fedora $upgrade_to upgrade, staying on Fedora $fedora." \
			"If packages conflict, try: sudo dnf system-upgrade download" \
			"--releasever=$upgrade_to --allowerasing"
	fi
}

explain_mok_enrollment() {
	reboot_for "enroll the module signing key, see above"
	say "Next boot: enroll the module signing key, or the NVIDIA driver won't load."
	echo "A blue MOK Manager screen waits 10 s for a key press. Press any key, then pick"
	echo "Enroll MOK > Continue > Yes, type the password you picked, and pick Reboot. If you miss"
	echo "it, run this script again and reboot."
}

# List what failed and what needs a reboot, and reboot on ENTER (ESC finishes without).
report_and_offer_reboot() {
	local key
	if [ -s "$FAILURES" ]; then
		printf '\e[1;91m== Failed ==\e[0m\n'
		sed 's/^/  - /' "$FAILURES"
	fi
	[ -s "$REBOOT_REASONS" ] || return 0
	echo "Reboot to apply:"
	sed 's/^/  - /' "$REBOOT_REASONS"
	say "Press ENTER to reboot, or ESC to finish without rebooting."
	# IFS= keeps a space or tab as the key read, rather than an empty one like ENTER.
	while IFS= read -r -s -n 1 key; do
		case $key in
		'')
			# A plain reboot doesn't install a downloaded Fedora upgrade.
			if [ -n "${fedora_upgrade:-}" ]; then
				sudo dnf offline reboot -y
			else
				sudo reboot
			fi
			break
			;;
		$'\e')
			# Swallow the rest of a key that sends an escape sequence, like an arrow key.
			read -r -s -t 0.1 -n 10
			[ -n "${fedora_upgrade:-}" ] &&
				echo "To upgrade to Fedora $upgrade_to later, run: sudo dnf offline reboot"
			break
			;;
		esac
	done
}
