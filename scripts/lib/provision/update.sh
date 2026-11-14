# shellcheck shell=bash
# shellcheck disable=SC2154 # $dotfiles is set by provision
#
# provision updating itself: pull the checkout's branch, and if that brought anything new, run
# the new provision in place of the running one.

# The host of the remote that the checkout's branch pulls from, or nothing for a local path.
dotfiles_remote_host() {
	local remote url
	remote=$(git -C "$dotfiles" config "branch.$(git -C "$dotfiles" branch --show-current).remote")
	url=$(git -C "$dotfiles" remote get-url "${remote:-origin}" 2>/dev/null) || return 0
	case $url in
	*://*) url=${url#*://} && url=${url%%/*} && url=${url##*@} && echo "${url%%:*}" ;;
	/* | ./* | ../*) ;;
	*:*) url=${url%%:*} && echo "${url##*@}" ;;
	esac
}

# Print why the remote can't be reached, if the reason is that there is no network: NetworkManager
# reports no internet connection, or the remote's host has no address.
offline_reason() {
	local host
	case $(nmcli -t -f CONNECTIVITY general status 2>/dev/null) in
	none | limited | portal) echo "no internet connection" && return ;;
	esac
	host=$(dotfiles_remote_host)
	[ -z "$host" ] || getent ahosts "$host" >/dev/null || echo "can't find $host"
}

# The update's one line of output, drawn over the line the cursor is on, in the columns and colors
# of the progress display (scripts/tui.py): an icon $2 in the color $1, "Checking for updates",
# and $3 after it.
update_line() { printf '\r   %b%s\e[0m \e[1mChecking for updates\e[0m%s\e[K' "$1" "$2" "$3"; }
update_done() { update_line '\e[1;92m' ✓ "$1" && echo; }

# Pull, with the output in the file $1, and spin meanwhile. Without prompts, which the spinner
# would draw over: if git or ssh needs to ask for something, this fails and says so.
pull_with_spinner() {
	local pid frame=0 spinner=(⠋ ⠙ ⠹ ⠸ ⠼ ⠴ ⠦ ⠧ ⠇ ⠏)
	GIT_TERMINAL_PROMPT=0 GIT_SSH_COMMAND="${GIT_SSH_COMMAND:-ssh} -o BatchMode=yes" \
		git -C "$dotfiles" pull --ff-only >"$1" 2>&1 &
	pid=$!
	if [ -t 1 ]; then
		# Hide the cursor while it spins, and give it back if interrupted.
		printf '\e[?25l'
		trap 'printf "\e[?25h\n"; exit 130' INT
		while kill -0 "$pid" 2>/dev/null; do
			update_line '\e[94m' "${spinner[frame++ % ${#spinner[@]}]}" ''
			sleep 0.08
		done
		trap - INT
		printf '\e[?25h'
	fi
	wait "$pid"
}

# Update the checkout, and run its provision with the arguments given if it changed. Only by
# fast-forward, so local commits and uncommitted changes are never merged or rewritten: a pull
# that would touch them stops here, as an old provision on a half-updated checkout helps nobody.
# Offline, the pull is skipped. PROVISION_UPDATED keeps the new provision, and the one run
# under the progress display, from pulling again.
update_dotfiles() {
	local offline before pull_log pull_status pulled
	[ -z "${PROVISION_UPDATED:-}" ] || return 0
	export PROVISION_UPDATED=1
	offline=$(offline_reason)
	if [ -n "$offline" ]; then
		update_done ": skipped, offline ($offline)"
		return
	fi
	before=$(git -C "$dotfiles" rev-parse HEAD 2>/dev/null)
	pull_log=$(mktemp)
	pull_with_spinner "$pull_log"
	pull_status=$?
	if [ $pull_status -ne 0 ] &&
		grep -qE 'Permission denied|Host key verification failed|terminal prompts disabled' "$pull_log"; then
		# It needed to ask, for a key's passphrase for instance. Again with prompts, and so
		# without the spinner.
		[ -t 1 ] && update_line '\e[94m' "⠋" '' && echo
		git -C "$dotfiles" pull --ff-only >"$pull_log" 2>&1
		pull_status=$?
	fi
	pulled=$(cat "$pull_log")
	rm -f "$pull_log"
	if [ $pull_status -ne 0 ]; then
		update_line '\e[1;91m' ✗ '' && echo
		# Indented to start under the line's text.
		{
			printf '\e[1;91m%s\e[0m\n' "Couldn't pull the latest dotfiles into $dotfiles."
			echo "git pull --ff-only said:"
			echo "${pulled:-(nothing)}" | sed 's/^/    /'
			echo "Resolve that by hand in $dotfiles, then run provision again."
			echo "To run this checkout as it is, without updating: provision -n"
		} | sed 's/^/     /' >&2
		exit 1
	fi
	if [ "$(git -C "$dotfiles" rev-parse HEAD)" != "$before" ]; then
		update_done ": updated to $(git -C "$dotfiles" log -1 --format='%h %s')"
		exec bash "$dotfiles"/provision "$@"
	fi
	update_done ''
}
