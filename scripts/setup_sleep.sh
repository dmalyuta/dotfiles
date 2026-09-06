#!/bin/bash
#
# Wake sources, and waking up from sleep with a working display.
#
# Run by start_fresh.sh, and safe to run on its own at any time: every step
# checks what is already in place and skips the expensive follow-up work
# (initramfs rebuild, grub regeneration, udev reload) when nothing changed.
#
#   ./scripts/setup_sleep.sh
#
# The files it installs live in .devices/ at the top of the repo rather than in
# heredocs, so they can be read, edited and linted on their own.
#
# ---------------------------------------------------------------------------
# 1. Coming back from suspend with a working display
# ---------------------------------------------------------------------------
#
# fbcon's takeover of the console is deferred by default, and a deferred
# takeover only resolves the first time something prints to the console --
# which, with "quiet", nothing does. So fbcon never binds to nvidia-drmdrmfb
# while the machine is up, and the takeover is still pending hours later when
# pm_restore_console switches VTs during resume. It then fires from the
# fbcon_register_existing_fbs workqueue, which takes console_lock and blocks
# inside nvidia_modeset, while systemd-sleep sits in pm_restore_console waiting
# for that same console_lock. systemd-sleep therefore never runs the post-sleep
# hook that tells the driver to restore video memory, and the driver never
# releases what fbcon is waiting for. Everything else resumes -- disks,
# network, userspace -- but the display stays dark and the console is wedged
# until the machine is power-cycled.
#
# fbcon=nodefer resolves the takeover at boot instead, taking it off the resume
# path entirely. Dropping "quiet" is a second, independent reason it now
# happens at boot. "splash" goes with it because Plymouth relies on the console
# staying off the framebuffer; once fbcon owns it at boot the two only fight.
#
# The alternative fix is nvidia_drm fbdev=0, which stops fbcon binding to the
# GPU at all. It works too, but it costs the text virtual terminals.
#
# A regression here looks like "PM: suspend entry" in the journal with no
# matching "PM: suspend exit", plus hung-task traces naming console_lock.
#
# NVreg_PreserveVideoMemoryAllocations and the nvidia sleep services are the
# other half: they write video memory to disk over the suspend rather than
# losing it.
#
# ---------------------------------------------------------------------------
# 2. Wake sources
# ---------------------------------------------------------------------------
#
# The Razer mouse must never wake the machine; the Keychron keyboard must.
# Enforced in two places on purpose:
#
#   - udev, so the state is right as soon as a device is enumerated.
#   - a pre-sleep hook, because the HID interface drivers probe *after* udev's
#     "add" event for the usb_device and re-enable wakeup behind udev's back.
#     That race is what made the original ACTION=="add" rule silently lose on
#     the Razer while looking installed correctly.
#
# ---------------------------------------------------------------------------
# 3. Making the next failure readable
# ---------------------------------------------------------------------------
#
# None of this changes how the machine sleeps. The noisier, higher-volume
# diagnostics are not installed here; scripts/sleep_diagnostics.sh turns those
# on and off.
#
# Author: Danylo Malyuta, 2026.

set -o pipefail

# Resolved from the script's own location, through any symlink, so it works the
# same whether it is run by path, from start_fresh.sh, or from $PATH.
script_dir=$(cd "$(dirname "$(readlink -f "${BASH_SOURCE[0]}")")" 2>/dev/null && pwd)
[ -n "$script_dir" ] || script_dir=$PWD
: "${DEVICES_DIR:=$(dirname "$script_dir")/.devices}"

info() { printf '   %s\n' "$*"; }
say() { printf '\n\033[1m== %s\033[0m\n' "$*"; }
die() {
	printf '\033[1;31m!!\033[0m %s\n' "$*" >&2
	exit 1
}

[ -d "$DEVICES_DIR" ] || die "No .devices directory at $DEVICES_DIR.
   It lives at the top of the repo, next to start_fresh.sh. Point DEVICES_DIR
   at it if the repo is somewhere unusual."

# Install one of the files from .devices/ to where it belongs on the system.
# Returns 0 only when the content actually changed, so callers can skip the
# expensive follow-up work on a re-run.
install_device_file() {
	local name=$1 dest=$2 mode=${3:-644} src
	src=$DEVICES_DIR/$name
	[ -f "$src" ] || die "missing $src"
	if sudo cmp -s "$src" "$dest" 2>/dev/null; then
		return 1
	fi
	sudo mkdir -p "$(dirname "$dest")"
	sudo install -m "$mode" "$src" "$dest" || die "could not install $dest"
	info "installed $dest"
}

# Rewrite the kernel command line in /etc/default/grub: make sure every option
# in $1 is present and every option in $2 is gone (both space-separated, plain
# words). Returns 0 only when the line actually changed, so callers can skip
# update-grub on a re-run.
kernel_cmdline() {
	local add=$1 drop=$2 key=GRUB_CMDLINE_LINUX_DEFAULT file=/etc/default/grub
	local cur new opt
	cur=$(sed -n "s/^${key}=\"\(.*\)\"$/\1/p" "$file")
	# Pad with spaces so every option is surrounded by them and the matches
	# below cannot catch a substring of a longer option.
	new=" $cur "
	for opt in $drop; do
		new=${new// $opt / }
	done
	for opt in $add; do
		case "$new" in *" $opt "*) ;; *) new="$new$opt " ;; esac
	done
	new=$(printf '%s' "$new" | tr -s ' ' | sed 's/^ //; s/ $//')
	[ "$new" = "$cur" ] && return 1
	if grep -q "^${key}=" "$file"; then
		sudo sed -i "s|^${key}=.*|${key}=\"${new}\"|" "$file"
	else
		printf '%s="%s"\n' "$key" "$new" | sudo tee -a "$file" >/dev/null
	fi
	info "kernel command line: $new"
}

say "Wake sources"

# Superseded by the single 90- rule; both were per-device and raced the HID
# drivers.
sudo rm -f /etc/udev/rules.d/razer-mouse.rules \
	/etc/udev/rules.d/keychron-keyboard.rules

if install_device_file 90-usb-wakeup.rules /etc/udev/rules.d/90-usb-wakeup.rules; then
	sudo udevadm control --reload
	# --reload only reloads the rule files; it does not reapply them to
	# devices that are already present, so trigger those explicitly.
	sudo udevadm trigger --action=add --subsystem-match=usb
fi

# Belt and braces for the rule above: re-assert the wake sources immediately
# before sleep, which is immune to enumeration order, replugs, dock power
# cycling, OpenRazer daemon restarts and driver rebinds.
#
# /usr/lib/systemd/system-sleep, not /etc/systemd/system-sleep: systemd only
# scans the former (it is the single system-sleep path in the systemd-sleep
# binary, and the only one man:systemd-sleep documents). A hook dropped in /etc
# is silently never run.
install_device_file usb-wakeup /usr/lib/systemd/system-sleep/usb-wakeup 0755

if [ -e /proc/driver/nvidia/version ]; then
	say "Nvidia suspend/resume"

	for unit in nvidia-suspend nvidia-hibernate nvidia-resume; do
		systemctl is-enabled --quiet "$unit.service" 2>/dev/null ||
			sudo systemctl enable "$unit.service"
	done

	if install_device_file zz-nvidia-local.conf /etc/modprobe.d/zz-nvidia-local.conf; then
		# Only rebuild the initramfs when the options actually changed.
		sudo update-initramfs -u
	fi

	if kernel_cmdline "fbcon=nodefer" "quiet splash"; then
		sudo update-grub
	fi
else
	info "No Nvidia driver loaded, skipping the resume fix."
fi

say "Diagnosability"

# journald syncs every 5 minutes by default, so a resume that has to be ended
# with the power button takes the whole record of itself with it.
# Earlier name for the same drop-in; leaving it behind means two files setting
# the same key.
sudo rm -f /etc/systemd/journald.conf.d/10-sleep-debug.conf
if install_device_file 10-sleep-sync.conf \
	/etc/systemd/journald.conf.d/10-sleep-sync.conf; then
	sudo systemctl restart systemd-journald
fi

# Alt+SysRq+W (blocked tasks) then +L (all-CPU backtraces) then +S (sync) is the
# only way to capture a wedged resume from the keyboard. Ubuntu's default mask
# leaves W and L out.
if install_device_file 60-sysrq.conf /etc/sysctl.d/60-sysrq.conf; then
	sudo sysctl --system >/dev/null
fi

# Names the device that woke the machine, and - the important part - forces the
# kernel's whole suspend/resume sequence to disk during early resume, before
# the graphics session has had any chance to wedge. A power-cycle after that
# point still leaves the log intact.
install_device_file zz-sleep-forensics \
	/usr/lib/systemd/system-sleep/zz-sleep-forensics 0755

# A resume that leaves the display dead usually leaves the machine perfectly
# reachable over the network, so keep a way in that does not need the screen.
# Deliberately scoped to the home LAN rather than opened to the world.
if ! dpkg-query -W -f='${Status}' openssh-server 2>/dev/null | grep -q 'ok installed'; then
	sudo apt install -y openssh-server
fi
systemctl is-enabled --quiet ssh 2>/dev/null || sudo systemctl enable --now ssh
if command -v ufw >/dev/null 2>&1; then
	sudo ufw allow from 192.168.4.0/22 to any port 22 proto tcp \
		comment 'ssh from LAN' >/dev/null
fi

say "Done"
info "Toggle the noisy diagnostics with scripts/sleep_diagnostics.sh on|off|status."
