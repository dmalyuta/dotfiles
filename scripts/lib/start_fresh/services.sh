# shellcheck shell=bash
# shellcheck disable=SC2034,SC2154 # shares variables with start_fresh and its other libraries
#
# start_fresh's configuration of apps and services: OpenRGB, OBS and its webcam microphone,
# Grafana, Syncthing, Flameshot and the system journal.

configure_openrgb() {
	sudo modprobe -a i2c-dev i2c-piix4
	printf 'i2c-dev\ni2c-piix4\n' | write_root_file /etc/modules-load.d/i2c.conf
	mkdir -p ~/.config/OpenRGB/profiles ~/.config/OpenRGB/plugins
	ln -sf "$dotfiles"/config/OpenRGB/{Configuration.json,OpenRGB.json} ~/.config/OpenRGB/
	ln -sf "$dotfiles"/config/OpenRGB/profiles/{blue.json,off.json} ~/.config/OpenRGB/profiles/
}

# The OBS profile and scene collection in config/obs.
obs_basic=~/.config/obs-studio/basic
configure_obs() {
	local d user_ini=~/.config/obs-studio/user.ini
	# Linked by directory, since OBS saves a file by renaming a new one over it. Existing ones are
	# kept as *.orig.
	mkdir -p "$obs_basic"
	for d in profiles scenes; do
		[ -d "$obs_basic/$d" ] && [ ! -L "$obs_basic/$d" ] && mv -T "$obs_basic/$d" "$obs_basic/$d.orig"
		ln -sfn "$dotfiles/config/obs/$d" "$obs_basic/$d"
	done
	# The repo has the recording paths under /home/user (the obs-profile git filter): make them
	# this user's.
	sed -i -E "s#=/home/user(/|\$)#=$HOME\\1#" "$dotfiles"/config/obs/profiles/desktop/basic.ini
	# Use that profile and scene collection, else OBS makes empty "Untitled" ones in the repo.
	# FirstRun skips the first-run auto-configuration wizard, which would overwrite the profile.
	kwriteconfig6 --file "$user_ini" --group General --key FirstRun true
	kwriteconfig6 --file "$user_ini" --group Basic --key Profile desktop
	kwriteconfig6 --file "$user_ini" --group Basic --key ProfileDir desktop
	kwriteconfig6 --file "$user_ini" --group Basic --key SceneCollection desktop_monitor
	kwriteconfig6 --file "$user_ini" --group Basic --key SceneCollectionFile desktop_monitor.json
}

# Print the USB webcam's microphone as "<USB vendor id> <product id> <PipeWire source>". udev
# marks a sound card as a webcam's when its USB device also has a video interface, and gives it the
# id that PipeWire names its sources by (with other characters turned into "_").
find_webcam_mic() {
	local card props id source
	for card in /sys/class/sound/card*; do
		props=$(udevadm info -q property "$card")
		grep -qx 'SOUND_FORM_FACTOR=webcam' <<<"$props" || continue
		id=$(sed -n 's/^ID_ID=//p' <<<"$props" | tr -c 'A-Za-z0-9_.\n-' _)
		source=$(pactl list short sources |
			awk -v prefix="alsa_input.$id." 'index($2, prefix) == 1 { print $2; exit }')
		[ -n "$source" ] || continue
		echo "$(sed -n 's/^ID_VENDOR_ID=//p' <<<"$props") $(sed -n 's/^ID_MODEL_ID=//p' <<<"$props")" \
			"$source"
		return
	done
	return 1
}

# An echo-cancelled webcam microphone for OBS, so calls played on the speakers don't leak into
# it, see config/obs/pipewire. And since that ties the speakers to the webcam's clock, a guard that
# replugs the webcam when its USB link fails, see scripts/bin/webcam-guard.
set_up_webcam_mic() {
	local vendor product mic new guard_changed=
	local conf=~/.config/pipewire/pipewire.conf.d/60-echo-cancel.conf
	read -r vendor product mic < <(find_webcam_mic)
	if [ -z "$mic" ]; then
		[ -e "$conf" ] ||
			say "No USB webcam found: plug it in and run this again for its echo-cancelled mic." >&2
		return
	fi
	new=$(mktemp)
	sed "s|@WEBCAM_MIC@|$mic|" "$dotfiles"/config/obs/pipewire/60-echo-cancel.conf >"$new"
	if cmp -s "$new" "$conf"; then
		rm -f "$new"
	else
		mkdir -p "${conf%/*}"
		# Renamed over it rather than written into it: earlier versions linked it into the repo.
		mv -f "$new" "$conf"
		systemctl --user restart pipewire pipewire-pulse wireplumber
	fi
	# The level that the scene's filters are tuned for. PipeWire takes a moment to bring the
	# device back after a restart.
	for _ in {1..20}; do
		pactl set-source-volume "$mic" 80% 2>/dev/null && break
		sleep 0.5
	done

	printf 'vendor=%s\nproduct=%s\n' "$vendor" "$product" |
		write_root_file /etc/webcam-guard.conf && guard_changed=1
	install_unit webcam-guard.service
	install_sleep_hook bin/webcam-guard
	install_command webcam-guard && guard_changed=1
	[ -z "$guard_changed" ] || sudo systemctl try-restart webcam-guard
	sudo systemctl enable --now webcam-guard
}

# Grafana needs to read /home.
configure_grafana() {
	# Do NOT chmod -R here.
	chmod o+rx "$HOME"
	if write_root_file /etc/systemd/system/grafana-server.service.d/override.conf <<'EOF'; then
[Service]
ProtectHome=false
EOF
		sudo systemctl daemon-reload
		sudo systemctl try-restart grafana-server
	fi
}

# Syncthing, as a user service. Its web UI is on 127.0.0.1:8384, and also at
# http://syncthing.localhost through a loopback-only proxy from port 80, see
# scripts/systemd/syncthing-gui-proxy.* (Syncthing takes *.localhost as local, so its check
# against DNS rebinding stays on). Sets $syncthing_changed if this run set any of it up.
set_up_syncthing() {
	local b unit proxy_changed=
	systemctl --user -q is-enabled syncthing.service 2>/dev/null || syncthing_changed=1
	systemctl --user enable --now syncthing.service
	for unit in syncthing-gui-proxy.socket syncthing-gui-proxy.service; do
		install_script_file "systemd/$unit" "/etc/systemd/system/$unit" && proxy_changed=1
	done
	# SELinux lets the proxy use only a few ports: these booleans let it listen on 80 and connect
	# to 8384.
	for b in systemd_socket_proxyd_bind_any systemd_socket_proxyd_connect_any; do
		[ "$(getsebool "$b" 2>/dev/null)" = "$b --> on" ] ||
			{ sudo setsebool -P "$b" 1 && proxy_changed=1; }
	done
	if [ -n "$proxy_changed" ]; then
		sudo systemctl daemon-reload
		sudo systemctl restart syncthing-gui-proxy.socket
		syncthing_changed=1
	fi
	sudo systemctl enable --now syncthing-gui-proxy.socket
}

# Flameshot's Wayland clipboard copy is lost outside Gnome, as the capture window closes too early.
# Run it under XWayland instead, both from autostart and from D-Bus activation.
configure_flameshot() {
	local autostart=~/.config/autostart/Flameshot.desktop
	if [ ! -f "$autostart" ]; then
		mkdir -p ~/.config/autostart
		cat >"$autostart" <<'EOF'
[Desktop Entry]
Name=flameshot
Icon=flameshot
Exec=flameshot
Terminal=false
Type=Application
X-GNOME-Autostart-enabled=true
EOF
	fi
	set_desktop_key "$autostart" Exec "env QT_QPA_PLATFORM=xcb flameshot"
	mkdir -p ~/.local/share/dbus-1/services
	cat >~/.local/share/dbus-1/services/org.flameshot.Flameshot.service <<'EOF'
[D-BUS Service]
Name=org.flameshot.Flameshot
Exec=/usr/bin/env QT_QPA_PLATFORM=xcb /usr/bin/flameshot
EOF
}

# A bigger system journal than the default 50 MB.
set_journal_size() {
	if write_root_file /etc/systemd/journald.conf.d/99-size.conf <<'EOF'; then
[Journal]
SystemMaxUse=256M
EOF
		sudo systemctl restart systemd-journald
	fi
}
