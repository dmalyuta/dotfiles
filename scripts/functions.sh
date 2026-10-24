# shellcheck shell=bash
#
# Helpers shared by start_fresh.sh, configure_hardware.sh and configure_kde.sh. Source it, don't run
# it. install_script_file reads $dotfiles, which the sourcing script sets to the repo root.
#
# Author: Danylo Malyuta, 2026.

# Status message, in bold bright blue so it stands out.
say() { printf '\e[1;94m== %s ==\e[0m\n' "$*"; }

skip() { say "$* already present, skipping."; }

installed() { pacman -Q "$1" >/dev/null 2>&1; }

# Whether this is the laptop, an ASUS ProArt PX13 (HN7306WU), rather than the desktop.
is_laptop() { [ "$(cat /sys/class/dmi/id/board_name 2>/dev/null)" = HN7306WU ]; }

# Write stdin to a root-owned file. Returns 0 only if the content changed.
write_root_file() {
	local dest=$1 tmp
	tmp=$(mktemp)
	cat >"$tmp"
	if sudo cmp -s "$tmp" "$dest" 2>/dev/null; then
		rm -f "$tmp"
		return 1
	fi
	sudo install -Dm644 "$tmp" "$dest"
	rm -f "$tmp"
}

# Install a file from scripts/. Returns 0 only if the content changed.
install_script_file() {
	local src=$dotfiles/scripts/$1 dest=$2 mode=${3:-644}
	sudo cmp -s "$src" "$dest" 2>/dev/null && return 1
	sudo install -Dm "$mode" "$src" "$dest"
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
