#!/bin/bash
#
# Open cardwire-gui, or show its window if it is already running. It does not check for a running
# copy itself, so each launch from the menu would start another process with its own tray icon.
#
# Author: Danylo Malyuta, 2026.

pid=$(pgrep -u "$USER" -xo cardwire-gui)
# Its tray icon is on D-Bus as org.kde.StatusNotifierItem-<pid>-<n>, and activating that shows the
# window.
[ -n "$pid" ] && tray=$(gdbus call --session --dest org.freedesktop.DBus \
	--object-path /org/freedesktop/DBus --method org.freedesktop.DBus.ListNames |
	grep -oP "org\.kde\.StatusNotifierItem-$pid-\d+" | head -1)
[ -n "${tray:-}" ] && exec gdbus call --session --dest "$tray" --object-path /StatusNotifierItem \
	--method org.kde.StatusNotifierItem.Activate 0 0 >/dev/null
exec cardwire-gui "$@"
