# shellcheck shell=bash
# shellcheck disable=SC2034,SC2154 # the want_* answers are read by the steps in provision
#
# provision's questions, all asked up front so that the rest runs unattended. The answers are
# remembered in $answers (see provision) as the defaults for the next run.

# Keep the sudo password fresh in the background, so a long step doesn't ask again partway through.
# Asks for it again only if it was forgotten since get_sudo.
start_sudo_session() {
	sudo -v || exit
	while kill -0 $$ 2>/dev/null && sleep 60; do sudo -n -v 2>/dev/null || break; done &
	sudo_keepalive=$!
}

# Yes/no question whose default is the last answer, remembered under key $1. A remembered answer
# is used without asking, unless with -u.
ask() {
	local key=$1 hint="[yN]" a
	if [ -z "$ask_again" ] && [ -n "${answers[$key]:-}" ]; then
		[ "${answers[$key]}" = y ]
		return
	fi
	[ "${answers[$key]:-}" = y ] && hint="[Yn]"
	read -p "$2 $hint " -r a
	[[ ${a:-${answers[$key]:-n}} =~ ^[Yy]$ ]] && answers[$key]=y || answers[$key]=n
	[ "${answers[$key]}" = y ]
}

# Ask for a line of text, remembered under key $1, with the last answer pre-filled for editing.
# Args: key, prompt, a function that checks the answer, and help to print before asking. A valid
# remembered answer is used without asking, unless with -u. Only for what can't be found on the machine.
ask_text() {
	local key=$1 prompt=$2 valid=${3:-true} help=${4:-}
	local value=${answers[$key]-}
	if [ -z "$ask_again" ] && [ -n "${answers[$key]+set}" ] && "$valid" "$value"; then
		return
	fi
	[ -z "$help" ] || echo "$help"
	while true; do
		read -e -i "$value" -p "$prompt " -r value || exit
		"$valid" "$value" && break
		echo "Invalid, try again."
	done
	answers[$key]=$value
}

# Whether $1 lists VS Code workspaces as "<letter>:<absolute path>,..." (or is empty), with distinct
# letters. Not C, which is the hotkey's own letter.
valid_workspaces() {
	local entry="[ABD-Z]:/[^,;]*[^,;/]"
	[[ $1 =~ ^($entry(,$entry)*)?$ ]] &&
		[ -z "$(tr , '\n' <<<"$1" | cut -c1 | sort | uniq -d)" ]
}

# Offer a newer Fedora release: the highest final version on Fedora's download page. Asked every
# run while there is one, even with the last answers reused. It's downloaded at the end, see download_fedora_upgrade.
ask_fedora_upgrade() {
	local newest a
	newest=$(curl -fsSL https://fedoraproject.org/releases.json 2>/dev/null |
		grep -oE '"version": *"[0-9]+"' | grep -oE '[0-9]+' | sort -n | tail -n 1)
	if [ "${newest:-0}" -gt "$fedora" ]; then
		read -p "Fedora $newest is out. Upgrade to it from Fedora $fedora? [Yn] " -r a
		[[ ${a:-y} =~ ^[Yy]$ ]] && upgrade_to=$newest
	fi
}

# Ask for a one-time password for enrolling the module signing key. Asked every run until the key
# is enrolled, and only its hash is kept, for this run only.
ask_mok_password() {
	local password again
	echo "Secure Boot is on, so the NVIDIA driver needs a signing key that the firmware trusts. Pick"
	echo "a one-time password for adding it: the next boot asks for it once."
	while true; do
		read -s -p "Key password: " -r password && echo
		read -s -p "Again: " -r again && echo
		[ -n "$password" ] && [ "$password" = "$again" ] && break
		echo "Empty or different, try again."
	done
	mok_hash=$(mokutil --generate-hash="$password")
}

# Ask everything, setting the want_* variables that the steps go by.
ask_questions() {
	local a

	ask_fedora_upgrade
	ask_held_upgrades

	[ -n "$(git config --global user.name)" ] ||
		{ read -p "Git name: " -r a && git config --global user.name "$a"; }
	[ -n "$(git config --global user.email)" ] ||
		{ read -p "Git email: " -r a && git config --global user.email "$a"; }

	[ -x "$superluminal_dir"/Superluminal ] ||
		{ ask superluminal "Install Superluminal profiler?" && want_superluminal=1; }

	# By hardware, without asking: the desktop's fan chip driver (nct6775) and OpenRGB, the ASUS
	# laptop's tools, and cardwire for a laptop's hybrid GPU.
	if is_laptop; then
		is_asus_laptop && want_asusctl=1
		has_nvidia && want_cardwire=1
	else
		want_nct6775=1
		want_openrgb=1
	fi
	if ! installed coolercontrol && ask cc_backup "Restore CoolerControl settings from backup?"; then
		while true; do
			read -e -p "Path to the CoolerControl backup file: " -r cc_backup
			cc_backup="${cc_backup/#\~/$HOME}"
			[ -f "$cc_backup" ] && break
			echo "$cc_backup not found, try again."
		done
	fi

	# Offered only when a Brother is on the network, as its drivers install for the model found.
	installed avahi-tools || sudo dnf install -y -q avahi-tools </dev/null
	read -r brother_model brother_host brother_ip < <(find_brother)
	if installed brscan-skey; then
		want_brother=1
	elif [ -n "$brother_model" ] &&
		ask brother "Found a Brother $brother_model on the network. Install its printer and scanner drivers?"; then
		want_brother=1
	fi

	# The OBS configuration, on the desktop. Already installed if its profiles link into the repo.
	if ! is_laptop; then
		if [ "$(readlink "$obs_basic/profiles")" = "$dotfiles/config/obs/profiles" ]; then
			want_obs=1
		elif ask obs "Install the custom OBS configuration?"; then
			want_obs=1
		fi
	fi

	is_laptop && ask sleep_debug "Turn on the sleep and hibernation debug log (scripts/sleep-debug)?" &&
		want_sleep_debug=1
	[ -n "${want_sleep_debug:-}" ] &&
		ask sleep_debug_trace \
			"Also trace hibernations (pm_trace)? Shows where a hang stopped; clock off after each until online." &&
		want_sleep_debug_trace=1

	[ -d "$pdfx_dir" ] || { ask pdfx "Install PDF-XChange Editor?" && want_pdfx=1; }
	command -v PureRef >/dev/null || want_pureref=1

	ask_text workspaces "Workspaces:" valid_workspaces \
		"VS Code workspaces for Meta+C, Meta+<letter>, as <letter>:<absolute path>,... with distinct
letters other than C and no trailing slashes, like M:/home/me/project,X:/home/me/other (empty
for none). Meta+<letter> is taken from whatever else uses it."

	if secure_boot && ! mok_key_ok; then
		ask_mok_password
	fi
}
