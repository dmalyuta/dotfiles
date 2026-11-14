# shellcheck shell=bash
# shellcheck disable=SC2034,SC2154 # shares variables with scripts/configure_hardware and its other libraries
#
# configure_hardware's sleep and hibernation setup for a laptop.

# Let the NVIDIA dGPU power off in sleep and whenever it's idle (runtime D3, see
# /proc/driver/nvidia/gpus/*/power). The laptop only has s2idle sleep, which reaches the SoC's
# deepest state only with every device off, and the driver keeps the dGPU on unless told.
#
# The driver's own sleep hook (UseKernelSuspendNotifiers) is off, as hibernation hung with it; RPM
# Fusion's nvidia-suspend, -hibernate and -resume services do its part instead. Keep systemd's
# default HibernateMode, platform: shutdown and reboot hang in the driver's nv_pci_shutdown.
set_nvidia_power_management() {
	write_root_file /etc/modprobe.d/nvidia-power.conf <<'EOF' &&
options nvidia NVreg_EnableS0ixPowerManagement=1
options nvidia NVreg_DynamicPowerManagement=0x02
options nvidia NVreg_UseKernelSuspendNotifiers=0
EOF
		reboot_for "NVIDIA power management options (/etc/modprobe.d/nvidia-power.conf)"
}

# An initramfs that can resume from hibernation: with dracut's resume module, which it leaves out
# without resume= on the kernel command line, and without the GPU drivers. The resumed kernel can't
# restore an iGPU that the initramfs set up (the screen stays black), and an NVIDIA driver loaded
# there refuses the freeze before the resume, so the image is dropped. RPM Fusion's
# 99-nvidia-dracut.conf already keeps NVIDIA's out.
set_up_initramfs_for_hibernation() {
	local rebuild=
	write_root_file /etc/dracut.conf.d/90-hibernate.conf <<'EOF' && rebuild=1
# Hibernation, see scripts/configure_hardware: resume, and without amdgpu, which can't resume
# from the initramfs.
add_dracutmodules+=" resume "
omit_drivers+=" amdgpu "
EOF
	# Also rebuild if a past rebuild was skipped or failed, so no initramfs still has a GPU driver.
	sudo find /boot -maxdepth 1 -name 'initramfs-*.img' ! -name '*rescue*' -exec lsinitrd {} \; \
		2>/dev/null | grep -q -E '/(nvidia|amdgpu)\.ko' && rebuild=1
	if [ -n "$rebuild" ]; then
		sudo dracut -f --regenerate-all
		reboot_for "initramfs rebuilt for hibernation"
	fi
}

# Hibernation goes to a swapfile as big as RAM (zram can't hold the image), in its own subvolume:
# btrfs can't snapshot one with an active swapfile. SELinux needs it labeled swapfile_t. systemd
# finds it on resume through an EFI variable, so no resume= is needed.
swapfile=/var/swap/swapfile
create_hibernation_swapfile() {
	local ram_gb
	ram_gb=$(awk '/MemTotal/ { print int($2 / 1048576) + 1 }' /proc/meminfo)
	sudo btrfs subvolume show /var/swap >/dev/null 2>&1 || sudo btrfs subvolume create /var/swap
	sudo semanage fcontext -l -C | grep -q '^/var/swap(/\.\*)?\s' ||
		sudo semanage fcontext -a -t swapfile_t '/var/swap(/.*)?'
	sudo btrfs filesystem mkswapfile --size "${ram_gb}g" "$swapfile"
	sudo restorecon -R /var/swap
	grep -q "^${swapfile}[[:space:]]" /etc/fstab ||
		{ cat /etc/fstab && echo "$swapfile none swap defaults 0 0"; } | write_root_file /etc/fstab
	sudo systemctl daemon-reload
	sudo swapon "$swapfile"
}

# Sleep, then hibernate after 50 min on battery (on AC it stays asleep), or with "off", back to
# plain sleep. By then sleeping has used more battery than a hibernation costs to enter and leave
# (measured with bin/sleep_stats).
sleep_then_hibernate() {
	local profile sleep_mode=(--delete)
	local sleep_conf=/etc/systemd/sleep.conf.d/50-suspend-then-hibernate.conf
	local suspend_dropin=/etc/systemd/system/systemd-suspend.service.d/50-suspend-then-hibernate.conf
	if [ "$1" = on ]; then
		write_root_file "$sleep_conf" <<'EOF'
[Sleep]
HibernateDelaySec=50min
HibernateOnACPower=no
EOF
		# The Plasma menu's Sleep asks logind for a plain suspend, so make that one sleep then
		# hibernate too.
		write_root_file "$suspend_dropin" <<'EOF' && sudo systemctl daemon-reload
[Service]
ExecStart=
ExecStart=/usr/lib/systemd/systemd-sleep suspend-then-hibernate
EOF
		# KDE's "Standby, then hibernate".
		sleep_mode=(3)
	else
		sudo rm -f "$sleep_conf"
		if [ -e "$suspend_dropin" ]; then
			sudo rm -f "$suspend_dropin"
			sudo systemctl daemon-reload
		fi
	fi
	for profile in AC Battery LowBattery; do
		kconf --file powerdevilrc --group "$profile" --group SuspendAndShutdown --key SleepMode \
			"${sleep_mode[@]}"
	done
}

# Pack and unpack the hibernation image on half of the CPU's threads instead of the kernel's 3: the
# compression is most of a hibernation's time and battery. On the command line rather than in
# /sys/power, so that the kernel that loads the image at resume uses them too.
set_hibernation_compression_threads() {
	local threads=$(($(nproc --all) / 2))
	# Older kernels don't have the option.
	[ -e /sys/power/hibernate_compression_threads ] && [ "$threads" -gt 3 ] || return 0
	if ! sudo grubby --info=DEFAULT |
		grep -q "^args=.*\\bhibernate_compression_threads=$threads\\b"; then
		sudo grubby --update-kernel=ALL --args="hibernate_compression_threads=$threads"
		reboot_for "kernel option hibernate_compression_threads=$threads, for faster hibernation"
	fi
}

# Skip GRUB's menu and its timeout at the boot that resumes a hibernation, see the menu part.
skip_boot_menu_on_resume() {
	installed grub2-tools || return 0
	if install_script_file grub/15_hibernate_resume /etc/grub.d/15_hibernate_resume 755 ||
		! sudo grep -qs 'hibernated' /boot/grub2/grub.cfg; then
		sudo grub2-mkconfig -o /boot/grub2/grub.cfg >/dev/null 2>&1 ||
			failed "Could not rebuild GRUB's menu (grub2-mkconfig), see the full output."
	fi
}

# Turn hibernation off, so KDE never offers it (it would if a big enough swap showed up).
disable_hibernation() {
	write_root_file /etc/systemd/sleep.conf.d/50-no-hibernate.conf <<'EOF'
[Sleep]
AllowHibernation=no
AllowSuspendThenHibernate=no
AllowHybridSleep=no
EOF
}
