# shellcheck shell=bash
# shellcheck disable=SC2034,SC2154 # shares variables with scripts/configure_hardware and its other libraries
#
# configure_hardware's settings for any machine: GPUs, kernel, clock, screen rotation, and the
# patched libraries.

# On a laptop with an AMD iGPU, show Vulkan apps only that one: Electron apps (Obsidian, VS Code)
# list every GPU through Vulkan at startup, which wakes the NVIDIA dGPU. bin/nvidia-run runs one
# on the dGPU. Not on the desktop, whose screens are on the NVIDIA card: an app drawing on the
# iGPU can't show its window there.
hide_dgpu_from_vulkan() {
	local conf=~/.config/environment.d/60-vulkan-igpu.conf
	local radeon_icd=/usr/share/vulkan/icd.d/radeon_icd.x86_64.json
	if is_laptop && [ -n "$nvidia_version" ] && lspci -d 1002: | grep -qE 'VGA|3D|Display'; then
		mkdir -p ~/.config/environment.d
		cat >"$conf" <<EOF
# Vulkan apps only see the AMD iGPU, so they don't wake the NVIDIA dGPU. nvidia-run uses the dGPU.
VK_DRIVER_FILES=$radeon_icd
VK_ICD_FILENAMES=$radeon_icd
EOF
	else
		rm -f "$conf"
	fi
}

# Zen 5's 32-bit RDSEED is broken until a microcode fix that needs a BIOS update, so the kernel
# prints "RDSEED32 is broken" on screen at every boot (and harmlessly turns it off), at emergency
# level, which only loglevel=0 keeps off the console. Panics still show. New kernels copy the
# option from the running one.
hide_rdseed_boot_message() {
	if journalctl -k -b 0 -q -g 'RDSEED32 is broken' >/dev/null 2>&1 &&
		! sudo grubby --info=DEFAULT | grep -q '^args=.*\bloglevel=0\b'; then
		sudo grubby --update-kernel=ALL --args=loglevel=0
		reboot_for "kernel option loglevel=0, which hides the RDSEED32 boot message"
	fi
}

# Go from the boot splash straight to the login screen. Plasma's login manager, unlike GDM, doesn't
# take the screen over from Plymouth: plymouth-quit.service ends the splash first, and the text
# console shows, black with a blinking cursor, until the login screen draws. So the splash's last
# frame stays on screen until then, and the console's cursor is hidden so it doesn't blink on top.
# New kernels copy the option from the running one.
seamless_boot_splash() {
	installed plymouth || return 0
	local changed=
	if write_root_file /etc/systemd/system/plymouth-quit.service.d/50-retain-splash.conf <<'EOF'; then
# The splash stays on screen until the login screen draws, see seamless_boot_splash in the dotfiles.
[Service]
ExecStart=
ExecStart=-/usr/bin/plymouth quit --retain-splash
EOF
		sudo systemctl daemon-reload
		changed=1
	fi
	if ! sudo grubby --info=DEFAULT | grep -q '^args=.*\bvt\.global_cursor_default=0\b'; then
		sudo grubby --update-kernel=ALL --args=vt.global_cursor_default=0
		changed=1
	fi
	[ -z "$changed" ] || reboot_for "the boot splash going straight to the login screen"
}

# The hardware clock in UTC, Linux's default, unless Windows is installed, which keeps it in local
# time: systemd never updates a local-time clock, so it's an hour off after each daylight saving
# change. Look for Windows itself, as its boot entry can outlive it.
keep_hardware_clock_in_utc() {
	if sudo test -e /boot/efi/EFI/Microsoft/Boot/bootmgfw.efi &&
		lsblk -nro FSTYPE | grep -qixE 'ntfs|bitlocker'; then
		[ "$(timedatectl show -p LocalRTC --value)" = yes ] ||
			say "Windows is installed: run 'sudo timedatectl set-local-rtc 1' if its clock is off."
	elif [ "$(timedatectl show -p LocalRTC --value)" = yes ]; then
		sudo timedatectl set-local-rtc 0
	fi
}

# Shut down or reboot within about half a minute whatever hangs (it takes 4 s when nothing does).
# When the NVIDIA driver breaks, what uses the GPU (cardwired, nvidia-powerd, KWin) gets stuck in
# the kernel where no signal reaches it, and systemd waited out every stage of stopping each: 4 min
# for cardwired alone, then 90 s more before the power-off. So:
# - 15 s after it starts, systemd stops waiting for units and goes on to power off (its own
#   fallback, which it ships set to 30 min): it still ends every process, syncs and unmounts.
# - A unit without a timeout of its own has 10 s to stop, not 45 s: the power-off waits that long
#   twice for processes that outlive their signals.
limit_shutdown_time() {
	local target changed=
	write_root_file /etc/systemd/system.conf.d/50-fast-shutdown.conf <<EOF && changed=1
# A hung service can't hold up a shutdown, see limit_shutdown_time in the dotfiles.
[Manager]
DefaultTimeoutStopSec=10s
EOF
	for target in poweroff reboot halt kexec; do
		write_root_file /etc/systemd/system/$target.target.d/50-fast-shutdown.conf <<EOF && changed=1
# A hung service can't hold up a shutdown, see limit_shutdown_time in the dotfiles.
[Unit]
JobTimeoutSec=15s
EOF
	done
	[ -z "$changed" ] || sudo systemctl daemon-reload
}

# More inotify instances per user than the kernel's 128. Every program that watches files for
# changes takes one or more (VS Code alone about 40), so VS Code, a browser, Obsidian and KDE's
# services together run out: file watching then stops working, and KDE warns that the capacity is
# low. Watches, the other inotify limit, are plentiful.
raise_inotify_limit() {
	write_root_file /etc/sysctl.d/90-inotify-instances.conf <<'EOF' || return 0
# More inotify instances per user, see raise_inotify_limit in the dotfiles.
fs.inotify.max_user_instances = 1024
EOF
	sudo sysctl -q -p /etc/sysctl.d/90-inotify-instances.conf
}

# GRUB boots the kernel picked last in its menu, not the newest: kernel-install doesn't make new
# kernels the default, and remember-kernel.service saves the booted one as the default. The menu
# shows at every boot, so a new kernel can be picked there. A resume from hibernation still boots
# the kernel that made the image, see scripts/sleep-hooks/hibernate-image.
remember_kernel_choice() {
	installed grub2-tools || return 0
	grep -qx 'UPDATEDEFAULT=yes' /etc/sysconfig/kernel 2>/dev/null &&
		sudo sed -i 's/^UPDATEDEFAULT=yes$/UPDATEDEFAULT=no/' /etc/sysconfig/kernel
	# Fedora hides the menu after a good boot when this is set.
	sudo grub2-editenv - list 2>/dev/null | grep -q '^menu_auto_hide=' &&
		sudo grub2-editenv - unset menu_auto_hide
	install_command remember-kernel
	install_unit remember-kernel.service
	sudo systemctl enable --now remember-kernel
}

# Rebuild GRUB's menu from /etc/default/grub and /etc/grub.d.
rebuild_grub_menu() {
	sudo grub2-mkconfig -o /boot/grub2/grub.cfg >/dev/null 2>&1 ||
		failed "Could not rebuild GRUB's menu (grub2-mkconfig), see the full output."
}

# Set a key in /etc/default/grub. Returns 0 only if it changed.
set_grub_default() {
	local file=/etc/default/grub
	grep -qxF "$1=$2" "$file" && return 1
	if grep -q "^$1=" "$file"; then
		sudo sed -i "s|^$1=.*|$1=$2|" "$file"
	else
		echo "$1=$2" | sudo tee -a "$file" >/dev/null
	fi
}

# The display scale of the sharpest connected display, guessed from its dpi (its native mode and
# its width in its EDID): 1 per 144 dpi, near what KDE picks. For when KDE can't be asked.
dpi_scale() {
	local d cm px
	for d in /sys/class/drm/card*-*; do
		[ "$(cat "$d/status" 2>/dev/null)" = connected ] || continue
		# sysfs gives the EDID's file size as 0, so just read it.
		cm=$(od -An -tu1 -j21 -N1 "$d/edid" 2>/dev/null | tr -d ' ')
		px=$(head -n 1 "$d/modes" 2>/dev/null)
		[ -n "$cm" ] && [ "$cm" -gt 0 ] && [ -n "$px" ] && echo "${px%%x*} $cm"
	done | awk '{ s = $1 / ($2 / 2.54) / 144; if (s > max) max = s } END { print max + 0 }'
}

# The pixel size at which the terminal (kitty) draws its $1-point font: 4/3 px a point, times the
# display scale KDE uses (the largest, if displays differ), or one guessed from the dpi without a
# KDE session to ask. Nothing if under 20 px: the firmware's own text (16 px) is as big there.
boot_menu_font_size() {
	local scale
	scale=$(kscreen-doctor -j 2>/dev/null | python3 -I -c '
import json, sys
print(max(o["scale"] for o in json.load(sys.stdin)["outputs"] if o["enabled"]))' 2>/dev/null) ||
		scale=$(dpi_scale)
	awk -v pt="$1" -v s="$scale" 'BEGIN { px = int(pt * 4 / 3 * s + 0.5); if (px >= 20) print px }'
}

# Draw GRUB's menu in the terminal's font (kitty's, from its config here) at the size the terminal
# shows it on screen (see boot_menu_font_size), in GRUB's own graphical terminal: on a high-dpi
# screen the firmware's text is tiny. Elsewhere, leave it in the firmware's text, and undo the font
# if a run on another screen set it.
size_boot_menu_font() {
	installed grub2-tools || return 0
	local conf=$dotfiles/config/kitty/kitty.conf family pt size font ttf
	local dir=/boot/grub2/fonts changed=
	family=$(sed -n 's/^font_family[[:space:]]*family="\([^"]*\)".*/\1/p' "$conf")
	family=${family:-monospace}
	pt=$(awk '$1 == "font_size" { print $2 }' "$conf")
	size=$(boot_menu_font_size "${pt:-12}")
	if [ -n "$size" ]; then
		font=$dir/dotfiles-${family//[^A-Za-z0-9]/}-$size.pf2
		if ! sudo test -s "$font"; then
			ttf=$(fc-match -f '%{file}' "$family:style=Regular")
			sudo rm -f "$dir"/dotfiles-*.pf2
			sudo mkdir -p "$dir"
			# Latin, arrows, and the box drawing of the menu's frame: the whole font is slow to load.
			if ! sudo grub2-mkfont -s "$size" -r 0x20-0x7E,0xA0-0xFF,0x2190-0x21FF,0x2500-0x259F \
				-o "$font" "$ttf"; then
				failed "Could not make GRUB's menu font from $ttf (grub2-mkfont)."
				return
			fi
		fi
		set_grub_default GRUB_FONT "$font" && changed=1
		set_grub_default GRUB_TERMINAL_OUTPUT '"gfxterm"' && changed=1
	elif grep -q "^GRUB_FONT=$dir/dotfiles-" /etc/default/grub; then
		sudo sed -i '/^GRUB_FONT=/d' /etc/default/grub
		set_grub_default GRUB_TERMINAL_OUTPUT '"console"'
		sudo rm -f "$dir"/dotfiles-*.pf2
		changed=1
	fi
	[ -z "$changed" ] || rebuild_grub_menu
}

# Remove what earlier versions installed and newer ones replaced: udev rules for the USB wake
# sources, and the webcam guard under its first name.
remove_replaced_files() {
	if [ -e /etc/udev/rules.d/90-usb-wakeup.rules ]; then
		sudo rm -f /etc/udev/rules.d/90-usb-wakeup.rules
		sudo udevadm control --reload
	fi
	if [ -e /etc/systemd/system/c920-guard.service ]; then
		sudo systemctl disable --now c920-guard
		sudo rm -f /etc/systemd/system/c920-guard.service /usr/local/bin/c920-guard \
			/usr/lib/systemd/system-sleep/c920-guard
		sudo systemctl daemon-reload
	fi
}

# Whether a HID device's report descriptor still ends with the field that the accelerometer fix
# removes (see the program's motion_state_tail).
has_motion_state_field() {
	[[ $(od -An -tx1 -v "$1/report_descriptor" | tr -d ' \n') == *0a510415002501750895018102c0 ]]
}

# Since kernel 7.1 the accelerometer's readings are dropped, so the screen doesn't rotate: a
# HID-BPF program fixes its report descriptor, see scripts/hid-bpf/amd-sfh-accel.bpf.c. Built
# against the running kernel's types (CO-RE, so later kernels load it too), and loaded by udev at
# boot.
install_accelerometer_fix() {
	local src=$dotfiles/scripts/hid-bpf/amd-sfh-accel.bpf.c
	local obj=/etc/udev-hid-bpf/amd-sfh-accel.bpf.o
	local build dev attached=
	# Its source is kept next to it, to rebuild only when it changes.
	if [ ! -f "$obj" ] || ! sudo cmp -s "$src" "${obj%.o}.c"; then
		build=$(mktemp -d)
		if bpftool btf dump file /sys/kernel/btf/vmlinux format c >"$build/vmlinux.h" &&
			clang -O2 -g -target bpf -Wno-missing-declarations -I"$build" \
				-c "$src" -o "$build/fix.bpf.o"; then
			sudo install -Z -Dm644 "$build/fix.bpf.o" "$obj"
			sudo install -Z -Dm644 "$src" "${obj%.o}.c"
		else
			failed "Could not build the accelerometer fix, so the screen won't rotate."
		fi
		rm -rf "$build"
	fi
	install_udev_rule 99-hid-bpf-amd-sfh-accel.rules
	[ -f "$obj" ] || return 0
	# Fix the accelerometer now too. It comes back as a new IIO device, so restart
	# iio-sensor-proxy.
	for dev in /sys/bus/hid/devices/0020:1022:0001.*; do
		[ -e "$dev/report_descriptor" ] && has_motion_state_field "$dev" &&
			sudo udev-hid-bpf add "$dev" "$obj" && attached=1
	done
	[ -z "$attached" ] || sudo systemctl restart iio-sensor-proxy
}

# Replace a library with one built with a patch from scripts/patches, again whenever an update
# puts Fedora's back, see patch-library. It takes effect at the next login. The patches' settings
# are in System Settings > Danylo's Linux Tweaks, see config/tweaks. Args: what goes wrong without
# the patch, the patch, its package, the library.
patch_library() { "$dotfiles"/scripts/patches/patch-library --impact "$@"; }
