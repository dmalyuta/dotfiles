# shellcheck shell=bash
#
# Helpers shared by start_fresh and the scripts it runs. Source it, don't run it. Some read
# $dotfiles, which the sourcing script sets to the repo root.
#
# Author: Danylo Malyuta, 2026.

# Status message, in bold bright blue so it stands out.
say() { printf '\e[1;94m== %s ==\e[0m\n' "$*"; }

# Report a failure, in bold red. Under start_fresh it also goes to the file $FAILURES, which it
# lists at the end.
failed() {
	printf '\e[1;91m== %s ==\e[0m\n' "$*" >&2
	[ -z "${FAILURES:-}" ] || echo "$*" >>"$FAILURES"
}

# Print a report for fixing a patch that failed. Args: what failed, the patch, the build log, the
# source kept when the patch didn't apply (or ""), the command that tries again.
patch_report() {
	local what=$1 patch=$2 log=$3 src=$4 retry=$5
	echo "$what"
	echo
	echo "Patch:     $patch (its description at the top says what it changes and why)"
	echo "Build log: $log"
	if [ -n "$src" ]; then
		echo "Source:    $src"
		echo "           Fedora's source, prepared as its build does, then committed to git before"
		echo "           the patch: \`git diff\` there shows the hunks that applied."
	fi
	echo
	echo "To fix it, update the patch for this source, along with its description (or drop it, if"
	echo "the package now fixes what the patch does). Then try again with:"
	echo "  $retry"
	echo
	if [ -n "$src" ] && [ -n "$(find "$src" -name '*.rej' -print -quit)" ]; then
		echo "== The hunks that didn't apply"
		find "$src" -name '*.rej' -printf '%P\n' | sort | while read -r rej; do
			echo "--- $rej"
			cat "$src/$rej"
		done
	else
		echo "== Errors in the build log"
		grep -E '(^|[^-[:alnum:]])error:|Error [0-9]|undefined reference' "$log" | head -n 40
		echo
		echo "== The end of the build log"
		tail -n 40 "$log"
	fi
}

# Note that a reboot is needed to apply $*. Under start_fresh it goes to the file $REBOOT_REASONS,
# which it lists at the end, else it's printed. Scripts that call this exit with status 2.
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
# scripts/tui.py reads these lines from the scripts beforehand, so the description must be a
# literal string. WEIGHT is its rough share of the time, 1 by default. Without the bar, it's
# printed.
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

installed() { rpm -q "$1" >/dev/null 2>&1; }

# Whether this is the laptop, an ASUS ProArt PX13 (HN7306WU), rather than the desktop.
is_laptop() { [ "$(cat /sys/class/dmi/id/board_name 2>/dev/null)" = HN7306WU ]; }

# Whether there is an NVIDIA GPU, and the version of its installed driver (empty if none).
has_nvidia() { lspci -d 10de: | grep -qE 'VGA|3D'; }
nvidia_version() { installed xorg-x11-drv-nvidia && rpm -q --qf '%{VERSION}' xorg-x11-drv-nvidia; }

secure_boot() { mokutil --sb-state 2>/dev/null | grep -q 'SecureBoot enabled'; }

# Whether the kernel allows hibernation. With Secure Boot on, Fedora's kernel locks itself down,
# which turns hibernation off: /sys/power/disk then reads "[disabled]".
can_hibernate() { [ -e /sys/power/disk ] && ! grep -q '\[disabled\]' /sys/power/disk; }

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
	# shellcheck disable=SC2154 # set by the sourcing script
	local src=$dotfiles/scripts/$1 dest=$2 mode=${3:-644}
	sudo cmp -s "$src" "$dest" 2>/dev/null && return 1
	sudo install -Z -Dm "$mode" "$src" "$dest"
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

kconf() {
	# --notify makes running programs reload the setting.
	kwriteconfig6 --notify "$@"
}
