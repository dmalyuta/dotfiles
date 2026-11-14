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
