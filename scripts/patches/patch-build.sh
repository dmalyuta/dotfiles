# shellcheck shell=bash
# shellcheck disable=SC2154 # $patch_file, $cache and $log are set by the sourcing script
#
# Shared by patch-library and ../wine/patch-combase: getting Fedora's source of an installed
# package, applying a patch to it, and reporting a patch that needs fixing. Source it after
# functions.sh. It reads the sourcing script's $patch_file, $cache (the build's folder) and $log.
#
# Author: Danylo Malyuta, 2026.

# Download source package $1 into folder $2 and print its path. Only from Fedora's source repos, and
# without input: dnf would otherwise ask to import other repos' keys, a question hidden in the log.
#
# $1 is an installed package, with or without its version. It is asked for as the source package
# that rpm says it was built from, like wine-11.21-1.fc45.src for wine-core: with just the source
# repos on, dnf can't tell which source a package like wine-core, named unlike it, comes from.
download_source_rpm() {
	local src
	src=$(rpm -q --qf '%{SOURCERPM}\n' "$1" 2>/dev/null | head -n 1)
	[[ $src == *.src.rpm ]] && src=${src%.rpm} || src=$1
	dnf download -q --repo=fedora-source --repo=updates-source --source --destdir "$2" "$src" \
		</dev/null >>"$log" 2>&1 || return
	compgen -G "$2/*.src.rpm" | head -n 1
}

# A progress bar for a build that writes to $log, drawn over one line of the terminal from
# progress_start until progress_stop or progress_done, with the time taken so far. How far along
# the build is comes from the log: the "[ 42%]" that CMake's builds print, the "[12/284]" of
# Meson's, or, for a build that prints neither, the lines logged since the start out of $1 of them
# (pass the count of `make -n`). Without a terminal there is no bar, just a line saying to wait.
progress_pid='' progress_t0=''
progress_start() {
	local total=${1:-} from pct best=0
	if [ ! -t 1 ]; then
		[ -n "$progress_t0" ] || echo "Compiling, this takes a while."
		progress_t0=${progress_t0:-$SECONDS}
		return
	fi
	# A second bar, for the next part of the same build, carries on the first one's clock.
	progress_t0=${progress_t0:-$SECONDS}
	from=$(wc -l <"$log")
	(
		# Until stopped, or until the script itself is gone.
		while kill -0 $$ 2>/dev/null; do
			pct=$(progress_percent "$total" "$from")
			# Never backwards: parallel jobs log out of order, and warnings push the marks away.
			[ "$pct" -gt "$best" ] && best=$pct
			progress_draw "$best"
			sleep 0.5
		done
	) &
	progress_pid=$!
}

# How far the build is, in percent: of $1 log lines since line $2, or by the log's last mark. At
# most 99, as the build still has to end.
progress_percent() {
	local total=$1 from=$2 last pct=0
	if [ -n "$total" ]; then
		[ "$total" -gt 0 ] && pct=$((($(wc -l <"$log") - from) * 100 / total))
	else
		last=$(tail -c 65536 "$log" | grep -aoE '^\[ *[0-9]+%\]|^\[[0-9]+/[0-9]+\]' | tail -n 1)
		last=${last//[][ ]/}
		case $last in
		*%) pct=${last%\%} ;;
		*/*) [ "${last#*/}" -gt 0 ] && pct=$((${last%/*} * 100 / ${last#*/})) ;;
		esac
	fi
	echo $((pct > 99 ? 99 : pct))
}

progress_time() { printf '%d:%02d' $(((SECONDS - progress_t0) / 60)) $(((SECONDS - progress_t0) % 60)); }

progress_draw() {
	local pct=$1 width=30 fill full rest
	fill=$((pct * width / 100))
	printf -v full '%*s' "$fill" ''
	printf -v rest '%*s' $((width - fill)) ''
	printf '\r  %s%s %3d%%  %s\e[K' "${full// /█}" "${rest// /░}" "$pct" "$(progress_time)"
}

# Take the bar away, as after a failed build.
progress_stop() {
	[ -n "$progress_pid" ] || return 0
	kill "$progress_pid" 2>/dev/null
	wait "$progress_pid" 2>/dev/null
	progress_pid=''
	printf '\r\e[K'
}

# The build worked: say how long it took, in place of the bar.
progress_done() {
	progress_stop
	echo "Compiled in $(progress_time)."
}

# Apply $patch_file to the source tree $1. If it no longer applies, fail, and keep the tree for
# fixing the patch: in $kept_source, in git, with the hunks that still apply applied.
kept_source=''
apply_patch() {
	local src=$1
	if patch -d "$src" -p1 --forward --dry-run <"$patch_file" >>"$log" 2>&1; then
		patch -d "$src" -p1 --forward -s <"$patch_file" >>"$log" 2>&1
		return
	fi
	git -C "$src" init -q && git -C "$src" add -A &&
		git -C "$src" -c user.name=patch-build -c user.email=none commit -qm "Fedora's source"
	patch -d "$src" -p1 --forward <"$patch_file" >>"$log" 2>&1
	mv "$src" "$cache/source" && kept_source=$cache/source
	return 1
}

# Print a report for fixing the patch. Args: what failed, the command that tries again.
patch_report() {
	local what=$1 retry=$2
	echo "$what"
	echo
	echo "Patch:     $patch_file (its description at the top says what it changes and why)"
	echo "Build log: $log"
	if [ -n "$kept_source" ]; then
		echo "Source:    $kept_source"
		echo "           Fedora's source, prepared as its build does, then committed to git before"
		echo "           the patch: \`git diff\` there shows the hunks that applied."
	fi
	echo
	echo "To fix it, update the patch for this source, along with its description (or drop it, if"
	echo "the package now fixes what the patch does). Then try again with:"
	echo "  $retry"
	echo
	if [ -n "$kept_source" ] && [ -n "$(find "$kept_source" -name '*.rej' -print -quit)" ]; then
		echo "== The hunks that didn't apply"
		find "$kept_source" -name '*.rej' -printf '%P\n' | sort | while read -r rej; do
			echo "--- $rej"
			cat "$kept_source/$rej"
		done
	else
		echo "== Errors in the build log"
		grep -E '(^|[^-[:alnum:]])error:|Error [0-9]|undefined reference' "$log" | head -n 40
		echo
		echo "== The end of the build log"
		tail -n 40 "$log"
	fi
}

# Save that report next to the build, and list the failure for provision. Args: what failed, the
# command that tries again, what goes wrong without the patch.
report_patch_failure() {
	local report=$cache/failure-report.txt
	mkdir -p "$cache"
	patch_report "$1" "$2" >"$report"
	failed "$3 To fix the patch, see ${report/#$HOME/\~}"
}
