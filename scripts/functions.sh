# shellcheck shell=bash
# shellcheck disable=SC2154 # $dotfiles is set by the sourcing script
#
# Helpers shared by start_fresh and the scripts it runs. Source it, don't run it. Some read
# $dotfiles, which the sourcing script sets to the repo root.
#
# Author: Danylo Malyuta, 2026.

# --------------------------------------------------------------------------------------------------
# Messages and progress.
# --------------------------------------------------------------------------------------------------

# Status message, in bold bright blue so it stands out.
say() { printf '\e[1;94m== %s ==\e[0m\n' "$*"; }

# Report a failure, in bold red. start_fresh also lists it at the end, from the file $FAILURES, and
# so does its progress display (scripts/tui.py) once it has cleared the output.
failed() {
	printf '\e[1;91m== %s ==\e[0m\n' "$*" >&2
	[ -z "${FAILURES:-}" ] || echo "$*" >>"$FAILURES"
	[ -z "${START_FRESH_STATUS:-}" ] || echo "failed $*" >>"$START_FRESH_STATUS"
}

# Note that a reboot is needed to apply $*. start_fresh lists these at the end, from the file
# $REBOOT_REASONS. Scripts that call this exit with status 2.
reboot_for() {
	# shellcheck disable=SC2034 # read by the scripts that source this
	need_reboot=1
	if [ -n "${REBOOT_REASONS:-}" ]; then
		echo "$*" >>"$REBOOT_REASONS"
	else
		say "Reboot needed: $*"
	fi
}

# Start a step: `step [WEIGHT] "What it does"`, which moves start_fresh's progress bar along.
# scripts/tui.py reads these lines from the scripts beforehand, in file order, so the description
# must be a literal string. WEIGHT is its rough share of the time, 1 by default.
step() {
	[[ $1 =~ ^[0-9]+$ ]] && shift
	if [ -n "${START_FRESH_STATUS:-}" ]; then
		echo "step $*" >>"$START_FRESH_STATUS"
	else
		say "$*"
	fi
}

# All steps are done: the progress bar fills.
steps_done() { [ -z "${START_FRESH_STATUS:-}" ] || echo "done" >>"$START_FRESH_STATUS"; }

# --------------------------------------------------------------------------------------------------
# What this machine is and has.
# --------------------------------------------------------------------------------------------------

installed() { rpm -q "$1" >/dev/null 2>&1; }

# Any machine with a battery.
is_laptop() { compgen -G '/sys/class/power_supply/BAT*' >/dev/null; }

# The one ASUS laptop model that the ASUS setup was made for: its fan curves, power limits and
# tablet-mode switch are specific to this board.
is_asus_laptop() { [ "$(cat /sys/class/dmi/id/board_name 2>/dev/null)" = HN7306WU ]; }

# A laptop that folds or detaches into a tablet (DMI chassis types 31 and 32).
is_convertible() { [[ $(cat /sys/class/dmi/id/chassis_type 2>/dev/null) == 3[12] ]]; }

# AMD's Sensor Fusion Hub, whose sensors are HID devices on its own bus (0x20).
has_amd_sfh() { compgen -G '/sys/bus/hid/devices/0020:1022:0001.*' >/dev/null; }

# An NVIDIA GPU. cardwire hides a blocked one from the PCI bus, but the driver still lists it.
has_nvidia() {
	lspci -d 10de: | grep -qE 'VGA|3D' || compgen -G '/proc/driver/nvidia/gpus/*' >/dev/null
}

# The version of the installed NVIDIA driver, empty if none.
nvidia_version() { installed xorg-x11-drv-nvidia && rpm -q --qf '%{VERSION}' xorg-x11-drv-nvidia; }

secure_boot() { mokutil --sb-state 2>/dev/null | grep -q 'SecureBoot enabled'; }

# Whether the kernel allows hibernation. With Secure Boot on, Fedora's kernel locks itself down,
# and /sys/power/disk then reads "[disabled]".
can_hibernate() { [ -e /sys/power/disk ] && ! grep -q '\[disabled\]' /sys/power/disk; }

# An answer that start_fresh remembered under key $1, empty if none. They are kept out of git, as
# some are private (project paths).
answer() {
	local -A answers=()
	# shellcheck source=/dev/null
	[ -f "$dotfiles/.start_fresh_answers" ] && . "$dotfiles/.start_fresh_answers"
	echo "${answers[$1]:-}"
}

# --------------------------------------------------------------------------------------------------
# Installing files.
# --------------------------------------------------------------------------------------------------

# Write stdin to a root-owned file. Returns 0 only if the content changed. -Z gives it the SELinux
# label for its path.
write_root_file() {
	local dest=$1 tmp
	tmp=$(mktemp)
	cat >"$tmp"
	if sudo cmp -s "$tmp" "$dest" 2>/dev/null; then
		rm -f "$tmp"
		return 1
	fi
	sudo install -Z -Dm644 "$tmp" "$dest"
	rm -f "$tmp"
}

# Install a file from scripts/. Returns 0 only if the content changed.
install_script_file() {
	local src=$dotfiles/scripts/$1 dest=$2 mode=${3:-644}
	sudo cmp -s "$src" "$dest" 2>/dev/null && return 1
	sudo install -Z -Dm "$mode" "$src" "$dest"
}

# Install scripts/bin/$1 as a command. Returns 0 only if it changed.
install_command() { install_script_file "bin/$1" "/usr/local/bin/$1" 755; }

# Install a script from scripts/ as a systemd sleep hook, run before and after each sleep. systemd
# only runs them from this folder.
install_sleep_hook() { install_script_file "$1" "/usr/lib/systemd/system-sleep/${1##*/}" 755; }

# Install the unit scripts/systemd/$1, and have systemd read it if it changed.
install_unit() {
	install_script_file "systemd/$1" "/etc/systemd/system/$1" && sudo systemctl daemon-reload
}

# Install the rules file scripts/udev/$1. If it changed, reload the rules, and with `udevadm
# trigger` options after the name, apply them to the devices already there.
install_udev_rule() {
	install_script_file "udev/$1" "/etc/udev/rules.d/$1" || return 0
	sudo udevadm control --reload
	[ $# -eq 1 ] || sudo udevadm trigger "${@:2}"
}

# Set a key in a .desktop file's [Desktop Entry] group.
set_desktop_key() {
	local file=$1 key=$2 value=$3 sudo_cmd=()
	[ -f "$file" ] || return 0
	[ -w "$file" ] || sudo_cmd=(sudo)
	if grep -q "^${key}=" "$file"; then
		"${sudo_cmd[@]}" sed -i "s|^${key}=.*|${key}=${value}|" "$file"
	else
		"${sudo_cmd[@]}" sed -i "/^\[Desktop Entry\]/a ${key}=${value}" "$file"
	fi
}

# Write a KDE setting. --notify makes running programs reload it.
kconf() { kwriteconfig6 --notify "$@"; }
