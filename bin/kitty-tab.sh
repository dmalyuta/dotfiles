#!/bin/bash
#
# Run a command in a new tab of the running kitty and bring its window to the front, or in a new
# kitty if none is running. A running kitty is found by the remote control socket that listen_on
# in kitty.conf opens, $XDG_RUNTIME_DIR/kitty-<pid>.
#
# Usage: kitty-tab.sh [--title TITLE] COMMAND [ARG...]
#
# Author: Danylo Malyuta, 2026.

title_args=()
if [ "${1:-}" = --title ]; then
	title_args=(--title "$2")
	shift 2
fi

# Activate the most recently used window of process $1, switching to its desktop. On Wayland a
# program cannot raise its own window unless it has focus, so a one-off KWin script does it.
kwin_activate() {
	local js name=kitty-tab-$$ id
	js=$(mktemp --suffix=.js) || return
	cat >"$js" <<EOF
const windows = workspace.stackingOrder;
for (let i = windows.length - 1; i >= 0; i--) {
    const w = windows[i];
    if (w.pid !== $1 || !w.normalWindow) {
        continue;
    }
    if (!w.onAllDesktops && !w.desktops.some(d => d.id === workspace.currentDesktop.id)) {
        workspace.currentDesktop = w.desktops[0];
    }
    w.minimized = false;
    workspace.activeWindow = w;
    break;
}
EOF
	id=$(gdbus call --session --dest org.kde.KWin --object-path /Scripting \
		--method org.kde.kwin.Scripting.loadScript "$js" "$name" | grep -oP '\d+')
	if [ -n "$id" ]; then
		gdbus call --session --dest org.kde.KWin --object-path /Scripting/Script"$id" \
			--method org.kde.kwin.Script.run >/dev/null
		gdbus call --session --dest org.kde.KWin --object-path /Scripting \
			--method org.kde.kwin.Scripting.unloadScript "$name" >/dev/null
	fi
	rm -f "$js"
}

# Newest kitty first. One that crashed leaves its socket behind.
while read -r sock; do
	pid=${sock##*-}
	kill -0 "$pid" 2>/dev/null || continue
	kitten @ --to "unix:$sock" launch --type=tab "${title_args[@]}" "$@" >/dev/null || continue
	kwin_activate "$pid"
	exit 0
done < <(ls -t "${XDG_RUNTIME_DIR:-/run/user/$UID}"/kitty-* 2>/dev/null)
exec kitty "${title_args[@]}" "$@"
