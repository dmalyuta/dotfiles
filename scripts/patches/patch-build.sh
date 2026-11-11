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
download_source_rpm() {
	dnf download -q --repo=fedora-source --repo=updates-source --source --destdir "$2" "$1" \
		</dev/null >>"$log" 2>&1 || return
	compgen -G "$2/*.src.rpm" | head -n 1
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

# Save that report next to the build, and list the failure for start_fresh. Args: what failed, the
# command that tries again, what goes wrong without the patch.
report_patch_failure() {
	local report=$cache/failure-report.txt
	mkdir -p "$cache"
	patch_report "$1" "$2" >"$report"
	failed "$3 To fix the patch, see ${report/#$HOME/\~}"
}
