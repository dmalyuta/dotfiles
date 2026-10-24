#!/bin/bash
#
# Low-level hardware setup: GPUs, kernel command line, initramfs, wake sources, sleep and
# hibernation, and the ASUS ProArt PX13 laptop's quirks. Run by start_fresh.sh after it installs
# the software, and safe to re-run on its own.
#
# Usage: configure_hardware.sh
#
# Exits with status 2 if a setting changed that takes a reboot to apply.
#
# Author: Danylo Malyuta, 2026.

dotfiles=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)

# --------------------------------------------------------------------------------------------------
# Helpers.
# --------------------------------------------------------------------------------------------------

. "$dotfiles"/scripts/functions.sh

nvidia_version=$(modinfo -F version nvidia 2>/dev/null)

# --------------------------------------------------------------------------------------------------
# GPUs.
# --------------------------------------------------------------------------------------------------

# Electron apps (Obsidian, VS Code, ...) list every GPU through Vulkan at startup, which wakes the
# NVIDIA dGPU for ~20 s on each launch. Show Vulkan apps only the AMD iGPU, which still accelerates
# them. bin/nvidia-run runs an app on the dGPU.
mkdir -p ~/.config/environment.d
cat >~/.config/environment.d/60-vulkan-igpu.conf <<'EOF'
# Vulkan apps only see the AMD iGPU, so they don't wake the NVIDIA dGPU. Use nvidia-run for the dGPU.
VK_DRIVER_FILES=/usr/share/vulkan/icd.d/radeon_icd.json
VK_ICD_FILENAMES=/usr/share/vulkan/icd.d/radeon_icd.json
EOF

# Also hide /dev/nvidiactl etc. when the dGPU is blocked, so tools like nvtop don't wake it. Only
# reliable with exactly one iGPU and one Nvidia dGPU.
installed cardwire && lspci -d 10de: | grep -qE 'VGA|3D' &&
	cardwire config experimental-nvidia-block true

# --------------------------------------------------------------------------------------------------
# Kernel.
# --------------------------------------------------------------------------------------------------

# Zen 5's 32-bit RDSEED is broken until a microcode fix. Without it, the kernel turns RDSEED off
# (harmless) and prints "RDSEED32 is broken. Please update your firmware." on screen at every boot.
# The fix ships in amd-ucode, but the CPU only accepts it once a BIOS with AMD's EntrySign fix is
# installed, so loading it from Linux fails silently. Until then, hide the message: it is logged at
# emergency level, which only loglevel=0 keeps off the console. Panics and oopses still show,
# since they raise the console log level themselves.
if journalctl -k -b 0 -q -g 'RDSEED32 is broken' >/dev/null 2>&1 &&
	! grep -q 'loglevel=0' /etc/default/limine; then
	sudo sed -i -E -e 's/ microcode\.amd_sha_check=off//' \
		-e 's/^(KERNEL_CMDLINE\[default\]\+?=".*)"$/\1 loglevel=0"/' /etc/default/limine
	rebuild_initramfs=1
fi

# --------------------------------------------------------------------------------------------------
# USB devices and wake sources.
# --------------------------------------------------------------------------------------------------

# Wake sources, and a working display after suspend. systemd only scans
# /usr/lib/systemd/system-sleep, not /etc.
if install_script_file 90-usb-wakeup.rules /etc/udev/rules.d/90-usb-wakeup.rules; then
	sudo udevadm control --reload
	sudo udevadm trigger --action=add --subsystem-match=usb
fi
install_script_file usb-wakeup /usr/lib/systemd/system-sleep/usb-wakeup 0755

# Replug the C920 webcam when its USB link fails, see scripts/c920-guard.
install_script_file c920-guard.service /etc/systemd/system/c920-guard.service && sudo systemctl daemon-reload
install_script_file c920-guard /usr/lib/systemd/system-sleep/c920-guard 0755
install_script_file c920-guard /usr/local/bin/c920-guard 0755 && sudo systemctl try-restart c920-guard
sudo systemctl enable --now c920-guard

# --------------------------------------------------------------------------------------------------
# Laptop sleep and hibernation.
# --------------------------------------------------------------------------------------------------

# Laptop sleep. Its only sleep state is s2idle, which reaches the SoC's deepest state only if every
# device powers off, and the Nvidia driver keeps the dGPU on unless told otherwise, draining ~1.5%/h.
#
# Also power the dGPU off whenever it is idle (runtime D3), even while KWin holds it open. 0x02
# makes this explicit; the default (0x03) also enables it on laptops. Check with
# /proc/driver/nvidia/gpus/*/power. Driver 615.78.08 reports it "Not supported" on kernel 7.3 but
# works on 6.18 LTS.
#
# Also turn off the driver's system sleep hook (UseKernelSuspendNotifiers). Since driver 595,
# nv_pci_shutdown tears the whole driver down, which hangs a hibernation that powers off by shutting
# devices down (HibernateMode=shutdown or reboot) in two ways: it waits forever for the display
# driver's power lock (nvkms_pm_lock), which the hook holds from the start of the sleep until the
# wake, and without the hook it waits forever on the dGPU, which the driver suspends before the image
# is written and leaves suspended. The default HibernateMode, platform, doesn't shut devices down
# (the driver's poweroff callback does nothing), and with the hook off it powers off and resumes
# cleanly; with the hook on it hung too, for a reason not tracked down. "Preserve all video memory"
# turns off with the hook, leaving the driver's default of saving the allocations it needs. The dGPU
# is mostly powered off anyway.
#
# Also keep Nvidia out of the initramfs, where chwd puts it. Resuming from hibernation happens in the
# initramfs, and the kernel must freeze its drivers before jumping into the image, which the freshly
# loaded Nvidia driver refuses (nv_pmops_freeze returns -EIO), so the image is dropped and the boot
# is fresh. The panel is on the iGPU, so the dGPU can load later.
if is_laptop && [ -n "$nvidia_version" ]; then
	sudo rm -f /etc/modprobe.d/nvidia-s0ix.conf
	write_root_file /etc/modprobe.d/nvidia-power.conf <<'EOF' && rebuild_initramfs=1
options nvidia NVreg_EnableS0ixPowerManagement=1
options nvidia NVreg_DynamicPowerManagement=0x02
options nvidia NVreg_UseKernelSuspendNotifiers=0
EOF
	write_root_file /etc/mkinitcpio.conf.d/90-no-nvidia.conf <<'EOF' && rebuild_initramfs=1
# Hibernation can't resume with Nvidia loaded in the initramfs, see scripts/configure_hardware.sh.
MODULES=(${MODULES[@]/#nvidia*/})
EOF
fi
# Keep amdgpu out of the initramfs too, by dropping the kms hook that adds it. Otherwise it sets up
# the iGPU before the resume, and the resumed kernel can't restore an iGPU that a different kernel
# set up: its firmware stops responding ("MES failed to respond", gfx_v11_0 -110) and the screen
# stays black. The boot splash draws on the firmware's framebuffer (simpledrm) instead.
if is_laptop; then
	write_root_file /etc/mkinitcpio.conf.d/90-no-kms.conf <<'EOF' && rebuild_initramfs=1
# Hibernation can't resume with amdgpu loaded in the initramfs, see scripts/configure_hardware.sh.
HOOKS=(${HOOKS[@]/kms/})
EOF
	# Also rebuild if a past rebuild was skipped or failed, so an initramfs still has a GPU driver.
	sudo find /boot -name 'initramfs-*' ! -name '*fallback*' -exec lsinitcpio {} \; 2>/dev/null |
		grep -q -E '/(nvidia|amdgpu)\.ko' && rebuild_initramfs=1
fi
# Also regenerates the boot entries, with the kernel command line from /etc/default/limine.
if [ -n "${rebuild_initramfs:-}" ]; then
	sudo limine-mkinitcpio
	need_reboot=1
fi

# Laptop hibernation, to a swapfile as big as RAM (zram can't hold the image). It goes in its own
# subvolume, since btrfs can't snapshot a subvolume with an active swapfile, and snapper snapshots
# @. The systemd initramfs finds it on resume through an EFI variable, so no resume= is needed.
swapfile=/swap/swapfile
if is_laptop && [ "$(findmnt -no FSTYPE /)" = btrfs ] && [ ! -f "$swapfile" ]; then
	root_dev=$(findmnt -no SOURCE / | sed 's/\[.*//')
	top=$(mktemp -d)
	sudo mount -o subvolid=5 "$root_dev" "$top"
	sudo btrfs subvolume show "$top/@swap" >/dev/null 2>&1 || sudo btrfs subvolume create "$top/@swap"
	sudo umount "$top" && rmdir "$top"
	{
		sudo cat /etc/fstab
		grep -q '[[:space:]]/swap[[:space:]]' /etc/fstab ||
			echo "UUID=$(findmnt -no UUID /) /swap btrfs subvol=/@swap,defaults,noatime 0 0"
		grep -q "^$swapfile[[:space:]]" /etc/fstab || echo "$swapfile none swap defaults 0 0"
	} | write_root_file /etc/fstab
	sudo systemctl daemon-reload
	sudo mkdir -p /swap
	mountpoint -q /swap || sudo mount /swap
	sudo btrfs filesystem mkswapfile --size "$(awk '/MemTotal/ { print int($2 / 1048576) + 1 }' \
		/proc/meminfo)g" "$swapfile"
	sudo swapon "$swapfile"
fi

# Sleep then hibernate after 3 h on battery, to cap the drain overnight. On AC it stays asleep, and
# the 3 h start when it is unplugged. SleepMode 3 is "Standby, then hibernate".
#
# Keep the default HibernateMode (platform), which the Nvidia driver needs, see above.
if is_laptop && [ -f "$swapfile" ]; then
	write_root_file /etc/systemd/sleep.conf.d/50-suspend-then-hibernate.conf <<'EOF'
[Sleep]
HibernateDelaySec=3h
HibernateOnACPower=no
EOF
	for profile in AC Battery LowBattery; do
		kconf --file powerdevilrc --group "$profile" --group SuspendAndShutdown --key SleepMode 3
	done
fi

# --------------------------------------------------------------------------------------------------
# ASUS laptop.
# --------------------------------------------------------------------------------------------------

# asusd sets the keyboard backlight, while systemd-backlight restores whatever it saved at
# shutdown, which can be 0. Also reset the keyboard controller after hibernation, see
# scripts/asus-kbd-rebind.
if installed asusctl; then
	sudo systemctl mask systemd-backlight@leds:asus::kbd_backlight.service
	install_script_file asus-kbd-rebind /usr/local/bin/asus-kbd-rebind 0755
	install_script_file asus-kbd-rebind.service /etc/systemd/system/asus-kbd-rebind.service &&
		sudo systemctl daemon-reload
	sudo systemctl enable asus-kbd-rebind
fi

# G-Helper's fan curves, see scripts/asus-fan-curves. asusd saves them.
if installed asusctl && is_laptop; then
	"$dotfiles"/scripts/asus-fan-curves >/dev/null
fi

# Per-profile CPU power limits, lower in Quiet for battery life, see scripts/asus-power-limits.
if installed acpi_call-dkms && is_laptop; then
	install_script_file asus-power-limits /usr/lib/systemd/system-sleep/asus-power-limits 0755
	install_script_file asus-power-limits.service /etc/systemd/system/asus-power-limits.service &&
		sudo systemctl daemon-reload
	install_script_file asus-power-limits /usr/local/bin/asus-power-limits 0755 &&
		sudo systemctl try-restart asus-power-limits
	sudo systemctl enable --now asus-power-limits
fi

# The PX13 is a 2-in-1, but asus-nb-wmi has no quirk for it, so it never reports tablet mode and KDE
# stays in laptop mode. It has the lid-flip sensor (WMI device 0x00060062), so use that. The switch
# is on "Asus WMI hotkeys", which scripts/keyd.conf leaves to libinput. Applies on next boot.
if is_laptop; then
	write_root_file /etc/modprobe.d/asus-tablet-mode.conf <<'EOF' && need_reboot=1
options asus_nb_wmi tablet_mode_sw=2
EOF
fi

[ -z "${need_reboot:-}" ] || exit 2
