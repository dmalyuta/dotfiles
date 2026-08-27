#!/bin/bash
#
# Sets up a high-performance Windows 11 VM on KVM with the AMD iGPU passed
# through, Looking Glass for a low-latency full-desktop view, and WinApps for
# running individual Windows programs as ordinary Linux windows.
#
# Run it directly:
#
#   ~/sw/dotfiles/make_windows_vm.sh
#
# or let start_fresh.sh offer it at the end of a fresh install.
#
# The work is split into numbered phases. Phase 2 reconfigures the host and
# needs a reboot, so the script records what it has finished in
# ~/.local/state/windows-vm and picks up where it left off on the next run.
# Every phase is safe to re-run: each one checks for what it creates and skips
# it if it is already there.
#
#   ./make_windows_vm.sh              run every phase that is not done yet
#   ./make_windows_vm.sh --status     show what is done and what is left
#   ./make_windows_vm.sh --only 2     run just phase 2, done or not
#   ./make_windows_vm.sh --from 3     run phase 3 onwards, done or not
#   ./make_windows_vm.sh --reset      forget all recorded progress
#   ./make_windows_vm.sh --config     every setting and its current value
#   ./make_windows_vm.sh --enable-console
#                                     put the emulated display back, if the
#                                     virtual one ever fails to appear
#
# On a machine that is not the one this was written for, start with --config.
# Everything it lists is read from the environment, so nothing here needs
# editing; the settings that describe hardware rather than taste are
#
#   IGPU_ID, IGPU_PCI, IGPU_AUDIO_ID, IGPU_AUDIO_PCI   lspci -Dnn
#   VM_CORES, VM_SIBLINGS, VM_EMULATOR_CPUS            lscpu -e
#   GOP_ROM_URL                                        a UEFI GOP driver
#                                                      matching this GPU
#
# Phase 0 checks all of them against the running machine before anything is
# reconfigured, so a wrong one is a message rather than a broken host.
#
# Author: Danylo Malyuta, 2026.

set -o pipefail

# ---------------------------------------------------------------------------
# Configuration. Every one of these can be overridden from the environment,
# e.g. VM_RAM_GB=16 ./make_windows_vm.sh
# ---------------------------------------------------------------------------

# Name libvirt knows the VM by.
: "${VM_NAME:=windows11}"

# Host CPUs the guest runs on, as "<first-thread-list> <sibling-thread-list>".
# These default to the second CCD of a 9950X3D: cores 8-15 with their SMT
# siblings 24-31. Windows gets a whole CCD to itself, so its threads never talk
# across the Infinity Fabric to each other, and Linux keeps the first CCD with
# the 96 MB of 3D V-Cache, which is the half that benefits from it most.
: "${VM_CORES:=8,9,10,11,12,13,14,15}"
: "${VM_SIBLINGS:=24,25,26,27,28,29,30,31}"

# Host CPUs for QEMU's own threads (emulator, iothread). Deliberately outside
# VM_CORES: these do the disk and network work, and letting them preempt a
# pinned vCPU is what shows up in the guest as audio crackle and frame stutter.
: "${VM_EMULATOR_CPUS:=0,16}"

# Guest RAM in GiB. Backed by hugepages that are allocated when the VM starts
# and handed back to Linux when it shuts down, so this is not subtracted from
# what the host can use while the VM is off.
: "${VM_RAM_GB:=32}"

# Guest system disk in GiB, and where the image lives. Kept out of $HOME on
# purpose: $HOME is shared into the guest over virtiofs, and a VM that can see
# its own backing image is a bad idea.
: "${VM_DISK_GB:=300}"
: "${VM_DISK:=/var/lib/libvirt/images/${VM_NAME}.raw}"

# The iGPU to hand to the guest. Vendor:device is what vfio-pci binds against;
# the PCI address is what the VM definition points at.
: "${IGPU_ID:=1002:13c0}"
: "${IGPU_PCI:=0000:79:00.0}"

# The iGPU's HDMI audio function, and whether to hand it to the guest too.
#
# On an integrated Radeon under UEFI firmware the graphics function alone is
# not enough: the guest driver stops with error code 43 in Device Manager. What
# is missing is a UEFI Graphics Output Protocol driver, and that is dealt with
# separately, by chaining one behind the VBIOS. See COMBINED_ROM below.
#
# So this is passed for its sound: it is the HDMI and DisplayPort audio that
# comes out of the same connectors as the picture. It shares an IOMMU group
# with the graphics function, which has to be passed through as a unit anyway.
#
# Set PASS_IGPU_AUDIO=no to go back to the graphics function on its own.
: "${PASS_IGPU_AUDIO:=yes}"
: "${IGPU_AUDIO_ID:=1002:1640}"
: "${IGPU_AUDIO_PCI:=0000:79:00.1}"

# The UEFI GOP driver.
#
# This machine has no such image of its own, so by default it comes from a
# collection of drivers pulled out of Ryzen firmware by a third party. Treat it
# as what it is: an unsigned firmware blob that the guest's UEFI executes
# before Windows starts. It runs inside the VM and never on the host, and the
# IOMMU keeps its DMA inside the guest, but nobody here has audited it. Point
# GOP_ROM_URL at a driver extracted from this board's own BIOS instead if that
# trade is not worth making.
: "${GOP_ROM_URL:=https://raw.githubusercontent.com/isc30/ryzen-gpu-passthrough-proxmox/main/AMDGopDriver_9950x3d.rom}"

# Where the VBIOS pulled out of the ACPI VFCT table gets written. An integrated
# GPU has no PCI ROM BAR to read a VBIOS from, so QEMU has to be handed one as a
# file or the guest driver cannot bring the card up.
: "${VBIOS_DIR:=/var/lib/libvirt/vbios}"
: "${VBIOS:=${VBIOS_DIR}/igpu-${VM_NAME}.rom}"
: "${GOP_ROM:=${VBIOS_DIR}/amd-gop-${VM_NAME}.rom}"

# The ROM the graphics function is actually handed: the VBIOS above with the
# GOP driver chained behind it.
#
# A PCI option ROM may hold several images, and firmware runs whichever one it
# understands. The VBIOS is a legacy x86 image, which OVMF cannot execute, so
# on its own it leaves the card unposted and the guest driver stops at error
# code 43. The GOP driver is the UEFI image OVMF wants. Chained, one file
# serves both: OVMF runs the GOP driver, and the AMD driver inside Windows
# still finds the VBIOS where it expects it.
: "${COMBINED_ROM:=${VBIOS_DIR}/igpu-combined-${VM_NAME}.rom}"

# The shared memory region Looking Glass moves frames through. Both sides have
# to agree on the name, so it is threaded from here into the domain XML, the
# AppArmor rules, the tmpfiles rule and the client launcher rather than being
# written out four times. The size has to hold two full frames plus a little
# overhead and must be a power of two: 3840x2160x4 bytes is 33 MB a frame, so
# 128 covers 4K comfortably.
: "${LG_SHM_NAME:=looking-glass}"
: "${LG_SHM_MB:=128}"

# Looking Glass. No distribution packages the client, so it is compiled here;
# the Windows host application and the IVSHMEM driver it needs arrive together
# in one signed installer from the same release.
: "${LG_SOURCE_URL:=https://looking-glass.io/artifact/stable/source}"
: "${LG_HOST_URL:=https://looking-glass.io/artifact/stable/host}"

# The Virtual Display Driver. The passed-through iGPU has no cable on it, so
# Windows gives it no display outputs and never draws on it. This invents a
# monitor and renders it on that GPU, which is what Looking Glass captures.
: "${VDD_URL:=https://github.com/VirtualDrivers/Virtual-Display-Driver/releases/download/25.7.23/VirtualDisplayDriver-x86.Driver.Only.zip}"

# The mode the virtual monitor comes up in. Left empty, phase 6 uses the size
# of the monitor this script is run on, so that the client draws guest pixels
# onto host pixels one for one instead of rescaling every frame.
: "${LG_WIDTH:=}"
: "${LG_HEIGHT:=}"

# What Windows calls the passed-through GPU. The virtual display driver is
# pointed at it by name, which is how its frames end up rendered on that card
# rather than on the emulated adapter.
: "${IGPU_GUEST_NAME:=AMD Radeon(TM) Graphics}"

# Give the GRUB menu a few seconds. Phase 2 puts vfio-pci.ids on the kernel
# command line, and if that ever goes wrong the fix is to edit it out at the
# boot menu, which is not reachable with the timeout at 0. Set to 0 to leave
# the existing GRUB timeout alone.
: "${GRUB_RECOVERY_TIMEOUT:=3}"

# Anything that is not a number is treated as "leave GRUB alone", rather than
# turning the arithmetic comparison in phase 2 into an error.
case $GRUB_RECOVERY_TIMEOUT in
'' | *[!0-9]*) GRUB_RECOVERY_TIMEOUT=0 ;;
esac

# The Windows 11 installation ISO. Left empty, phase 3 goes looking for one in
# the usual download directories and checks that what it finds is really a
# Windows installer. Microsoft's download API is gated behind an anti-bot
# check that rejects scripted requests, so this is a file you fetch yourself
# from https://www.microsoft.com/software-download/windows11 rather than
# something the script can reliably grab.
: "${WIN_ISO:=}"

# Which edition to install. Has to match an image name inside the ISO exactly:
# a multi-edition ISO carries Home, Education, Pro and their N variants, and
# the answer file picks one by name. Pro is the one that matters here, because
# Home has no RDP server and RDP is what puts individual Windows programs on
# your Linux desktop later.
: "${WIN_EDITION:=Windows 11 Pro}"

# Guest locale and timezone for the unattended install.
: "${WIN_LOCALE:=en-US}"
: "${WIN_TIMEZONE:=Pacific Standard Time}"

# The account the installer creates, which is also the account RDP logs in as.
: "${WIN_USER:=$USER}"
: "${WIN_HOSTNAME:=${VM_NAME^^}}"

# Where the launchers, desktop entries and icons this script generates go.
# These are the XDG defaults; change them only if your desktop looks elsewhere.
: "${BIN_DIR:=$HOME/.local/bin}"
: "${APP_DIR:=$HOME/.local/share/applications}"
: "${ICON_DIR:=$HOME/.local/share/icons/hicolor/256x256/apps}"

# The directory shared into the guest over virtiofs. Deliberately not $HOME:
# whatever is listed here is fully readable and writable by Windows, so it is
# the one place where a compromised guest could reach your Linux files. Keep
# the things you actually want in both worlds here and nothing else.
: "${VM_SHARE_DIR:=$HOME/Shared}"

# The paravirtualised Windows drivers. Windows Setup cannot see a virtio disk
# without these, so this ISO is attached during installation and its storage
# driver is loaded before the disk is partitioned.
: "${VIRTIO_URL:=https://fedorapeople.org/groups/virt/virtio-win/direct-downloads/stable-virtio/virtio-win.iso}"
: "${VIRTIO_ISO:=/var/lib/libvirt/images/virtio-win.iso}"

# WinFsp, which is what lets Windows mount a userspace filesystem at all. The
# virtio guest tools ship the shared-folder driver but not this, so without it
# the service exists and refuses to start, and no shared drive appears.
: "${WINFSP_URL:=https://github.com/winfsp/winfsp/releases/download/v2.1/winfsp-2.1.25156.msi}"

# Where the generated answer file ISO goes. Windows Setup looks for
# autounattend.xml in the root of every attached drive, so this rides along as
# a second CD-ROM and drives the whole installation unprompted.
: "${UNATTEND_ISO:=/var/lib/libvirt/images/${VM_NAME}-unattend.iso}"

# How virsh is invoked. The VM lives on the system libvirt instance, not the
# per-user one, so every call has to say so.
: "${VIRSH:=virsh --connect qemu:///system}"

# Where progress is recorded between runs.
# The long files this script installs — the Windows answer file, the libvirt
# hook, the two Python helpers — live beside it rather than inline, because a
# 200-line XML document wedged into a heredoc is unreadable and unlintable.
# Resolved from the script's own location, through any symlink, so it works the
# same whether it is run by path, from start_fresh.sh, or from $PATH.
script_dir=$(cd "$(dirname "$(readlink -f "${BASH_SOURCE[0]}")")" 2>/dev/null && pwd)
# Only fails when the script is not a real file on disk, such as when it is
# sourced from a pipe. Falling back to the working directory beats carrying an
# empty path forward and looking for templates in /.
[ -n "$script_dir" ] || script_dir=$PWD
: "${TEMPLATE_DIR:=$script_dir/.windows}"

state_dir=${XDG_STATE_HOME:-$HOME/.local/state}/windows-vm

# Written once Windows is known to be installed and answering. Its presence is
# what stops a later run from attaching the installation media again: phase 4
# is otherwise happy to boot the installer, and the answer file wipes the disk
# without asking, so a routine redefine would destroy a working system.
installed_marker=$state_dir/windows-installed

# The product key and the Windows account password, asked for once and kept
# here, mode 600, so re-runs do not keep prompting. They are deliberately not
# in the repo and not in the generated ISO's directory.
key_file=$state_dir/product-key
pw_file=$state_dir/windows-password

# Kernel command line arguments phase 2 adds.
#   amd_iommu=on      turn on the IOMMU, which is what isolates the passed
#                     through device's DMA from the rest of the machine
#   iommu=pt          leave host devices in passthrough mode, so they skip
#                     address translation they do not need and only the
#                     assigned device pays for it
#   vfio-pci.ids=...  claim the iGPU for vfio-pci during boot, before amdgpu
#                     gets a chance to bind to it
# Both functions of the card have to be claimed at boot, not just the graphics
# one: leaving the audio function on the host's driver means it cannot be given
# to the guest, and the GOP driver rides on its option ROM.
vfio_ids=$IGPU_ID
[ "$PASS_IGPU_AUDIO" = yes ] && vfio_ids="${vfio_ids},${IGPU_AUDIO_ID}"

kernel_args=(amd_iommu=on iommu=pt "vfio-pci.ids=${vfio_ids}")

# ---------------------------------------------------------------------------
# Helpers.
# ---------------------------------------------------------------------------

say() { printf '\n\033[1;34m==\033[0m \033[1m%s\033[0m\n' "$*"; }
info() { printf '   %s\n' "$*"; }
skip() { printf '   \033[2m%s already done, skipping.\033[0m\n' "$*"; }
warn() { printf '\033[1;33m!!\033[0m %s\n' "$*" >&2; }
die() {
	printf '\033[1;31m!!\033[0m %s\n' "$*" >&2
	exit 1
}

have() { command -v "$1" >/dev/null 2>&1; }

# True while the guest is up. Six callers asked libvirt this the long way.
vm_running() {
	$VIRSH domstate "$VM_NAME" 2>/dev/null | grep -q running
}

# Split a PCI address like 0000:79:00.1 into the hex fields libvirt's XML wants,
# leaving them in pci_domain, pci_bus, pci_slot and pci_func. These used to be
# sliced out by character offset, which turns an address written the short way
# (79:00.1) into a plausible-looking pointer at the wrong device.
pci_split() {
	[[ "$1" =~ ^([0-9a-fA-F]{4}):([0-9a-fA-F]{2}):([0-9a-fA-F]{2})\.([0-7])$ ]] ||
		die "'$1' is not a PCI address in DDDD:BB:SS.F form, e.g. 0000:79:00.0.
   'lspci -D' lists them the way they are wanted here."
	pci_domain=0x${BASH_REMATCH[1]}
	pci_bus=0x${BASH_REMATCH[2]}
	pci_slot=0x${BASH_REMATCH[3]}
	pci_func=0x${BASH_REMATCH[4]}
}

# Expand a cpuset list like "0-3,16" into the CPU numbers it names. libvirt
# takes ranges anywhere a cpuset is wanted, so a check that only understands
# comma-separated singletons would reject a perfectly good VM_EMULATOR_CPUS.
cpuset_expand() {
	local part lo hi
	for part in ${1//,/ }; do
		case $part in
		*-*)
			lo=${part%%-*} hi=${part##*-}
			[[ "$lo" =~ ^[0-9]+$ && "$hi" =~ ^[0-9]+$ ]] || return 1
			[ "$lo" -le "$hi" ] || return 1
			seq "$lo" "$hi"
			;;
		*)
			[[ "$part" =~ ^[0-9]+$ ]] || return 1
			echo "$part"
			;;
		esac
	done
}

# The two CPU lists have to pair up one for one and name CPUs this machine
# really has. build_vcpupin runs inside a command substitution, where a die
# would only end the subshell and leave the domain silently unpinned, so the
# check lives out here where it can stop the run.
check_cpu_lists() {
	local -a cores siblings
	IFS=',' read -ra cores <<<"$VM_CORES"
	IFS=',' read -ra siblings <<<"$VM_SIBLINGS"

	[ "${#cores[@]}" -gt 0 ] && [ "${#cores[@]}" -eq "${#siblings[@]}" ] ||
		die "VM_CORES has ${#cores[@]} entries and VM_SIBLINGS has ${#siblings[@]}.
   They are the two halves of the same physical cores, so they pair up one
   for one."

	# VM_CORES and VM_SIBLINGS are taken one entry at a time, because
	# build_vcpupin pairs them off; a range in either would silently collapse
	# eight vCPUs into one. VM_EMULATOR_CPUS is copied into the XML whole, so
	# it may hold ranges and is expanded before checking.
	local emu
	emu=$(cpuset_expand "$VM_EMULATOR_CPUS") ||
		die "VM_EMULATOR_CPUS is '$VM_EMULATOR_CPUS', which is not a cpuset
   list. It wants CPU numbers and ranges, as in 0,16 or 0-1,16-17."

	local cpu
	for cpu in "${cores[@]}" "${siblings[@]}" $emu; do
		[ -d "/sys/devices/system/cpu/cpu$cpu" ] ||
			die "This machine has no cpu$cpu, only $(nproc). VM_CORES and
   VM_SIBLINGS want individual CPU numbers, comma-separated and paired off
   one for one; 'lscpu -e' shows which threads are siblings."
	done

	info "Pinning ${#cores[@]} cores (${#cores[@]} x 2 threads) to the guest."
}

# True if the named apt package is installed.
pkg_installed() {
	dpkg-query -W -f='${Status}' "$1" 2>/dev/null | grep -q 'ok installed'
}

# Run a command as root. Prefers sudo, but falls back to pkexec when there is no
# terminal to type a password into, which is what happens when the script is
# driven by a tool rather than typed at a prompt. pkexec asks the desktop's
# polkit agent instead, so it works in a graphical session either way.
as_root() {
	if [ "$(id -u)" -eq 0 ]; then
		"$@"
	elif sudo -n true 2>/dev/null; then
		sudo "$@"
	elif [ -t 0 ]; then
		sudo "$@"
	elif have pkexec; then
		pkexec "$@"
	else
		die "need root for: $*"
	fi
}

# Write stdin to a root-owned file. Returns 0 only when the contents actually
# changed, so callers can skip the slow follow-up work (rebuilding the
# initramfs, regenerating the GRUB config) when a re-run changes nothing.
write_root_file() {
	local dest=$1 mode=${2:-644} tmp
	tmp=$(mktemp)
	cat >"$tmp"

	# A literal ${NAME} that reached here means the content was built with the
	# expansion switched off — a heredoc whose delimiter was quoted, as in
	# <<-'EOF', or a variable that is not set. render_template refuses to emit
	# an unfilled {{...}} for the same reason: a placeholder written into
	# /etc is not noticed until whatever reads it misbehaves, which for the
	# vfio-pci ids means after a reboot, with the GPU back on amdgpu.
	#
	# Braced names only, and only ones starting with a letter or underscore.
	# A bare $name cannot be told apart from ordinary text, and ${1} is a
	# positional parameter that the libvirt hook written through here uses
	# legitimately. That leaves the braced form, which is what every caller
	# here interpolates with anyway.
	if grep -qE '\$\{[A-Za-z_][A-Za-z0-9_]*\}' "$tmp"; then
		# One line per distinct variable, at the first place it appears, in
		# file order. sort -u would collapse the same set but reorder it.
		grep -onE '\$\{[A-Za-z_][A-Za-z0-9_]*\}' "$tmp" |
			awk -F: '!seen[$2]++ {print "     line " $1 ": " $2}' >&2
		rm -f "$tmp"
		die "Unexpanded variables in the content for $dest, listed above.
   Nothing was written. If that content came from a heredoc, its delimiter
   is quoted and nothing in the body expands; drop the quotes around it."
	fi

	if as_root cmp -s "$tmp" "$dest" 2>/dev/null; then
		rm -f "$tmp"
		return 1
	fi
	as_root mkdir -p "$(dirname "$dest")"
	as_root install -m "$mode" "$tmp" "$dest"
	rm -f "$tmp"
	return 0
}

# Append a block of lines to a root-owned config file, once. The marker comment
# is what makes a re-run a no-op, and what makes the block findable later by
# someone wondering where these lines came from. Returns 0 if the file changed.
append_root_block() {
	local dest=$1 marker=$2 body tmp
	body=$(cat)
	if as_root grep -qF "$marker" "$dest" 2>/dev/null; then
		return 1
	fi
	tmp=$(mktemp)
	as_root cat "$dest" 2>/dev/null >"$tmp"
	{
		echo
		echo "$marker"
		echo "$body"
	} >>"$tmp"
	as_root install -m 644 "$tmp" "$dest"
	rm -f "$tmp"
	return 0
}

# Phase bookkeeping.
phase_done() { [ -f "$state_dir/phase-$1.done" ]; }
mark_done() {
	mkdir -p "$state_dir"
	date -Is >"$state_dir/phase-$1.done"
}

# Ask a yes/no question, defaulting to no.
#
# Reads /dev/tty rather than stdin, for the same reason ask_secret does: the
# script is sometimes driven by another program, and read's own -p prompt is
# printed only when stdin is a terminal. Without this a non-interactive run
# stops at the first warning having shown no question and given no reason.
confirm() {
	local answer
	if ! have_tty; then
		warn "Cannot ask \"$*\" — no terminal. Taking that as no."
		warn "Run this in a terminal to answer it."
		return 1
	fi
	printf '%s [yN] ' "$*" >/dev/tty
	IFS= read -r answer </dev/tty
	[[ "$answer" =~ ^[Yy]$ ]]
}

# Print a template from TEMPLATE_DIR with its {{PLACEHOLDER}} tokens filled in.
# Takes the file name, then alternating name/value pairs:
#
#   render_template libvirt-qemu-hook.sh VM_NAME "$VM_NAME" VM_RAM_GB 32
#
# The replacement is quoted deliberately. Unquoted, bash 5.2 and later treat a
# bare & in it as "whatever the pattern matched", exactly the way sed does, and
# every value here that has been through xml_escape is full of &amp; and &lt;.
render_template() {
	local name=$1 file body key value pattern
	shift

	file=$TEMPLATE_DIR/$name
	[ -f "$file" ] || {
		warn "Missing template: $file"
		return 1
	}
	body=$(cat "$file") || return 1

	while [ $# -gt 1 ]; do
		key=$1 value=$2
		shift 2
		pattern="{{$key}}"
		body=${body//"$pattern"/"$value"}
	done

	# A placeholder nobody filled in would otherwise be written out verbatim
	# and only noticed when Windows Setup ignored the answer file.
	if grep -qE '\{\{[A-Z_]+\}\}' <<<"$body"; then
		warn "Template $name still has unfilled placeholders:"
		grep -oE '\{\{[A-Z_]+\}\}' <<<"$body" | sort -u | sed 's/^/     /' >&2
		return 1
	fi

	printf '%s\n' "$body"
}

# True when there is a terminal to prompt on. The script is sometimes driven by
# another program rather than typed at a prompt, and in that case there is
# nothing to read a password from and it has to say so rather than hang.
# Testing the file is not enough: /dev/tty exists and looks readable even in a
# process with no controlling terminal, and only opening it reveals that there
# is nothing behind it.
have_tty() { (exec 3<>/dev/tty) 2>/dev/null; }

# Ask for a secret once and remember it, mode 600, so re-runs never re-prompt.
# Reads and writes /dev/tty directly rather than stdin and stdout, so the
# prompt still reaches the user when the script's own streams are redirected,
# and the typed value stays off the screen and out of the shell history.
#
#   ask_secret <file> <prompt> [<validation-regex> <complaint>]
#
# An empty answer is accepted and stored as an empty file, which callers treat
# as "not supplied" — that is how you decline to give a product key.
ask_secret() {
	local file=$1 prompt=$2 pattern=${3:-} complaint=${4:-} value="" again=""

	[ -e "$file" ] && return 0

	have_tty || die "$prompt
   No terminal to ask on. Either run this script directly in a terminal, or
   put the value in $file yourself:
       install -m 600 /dev/null $file
       \$EDITOR $file"

	mkdir -p "$(dirname "$file")"
	chmod 700 "$(dirname "$file")"

	while :; do
		printf '%s' "$prompt" >/dev/tty
		IFS= read -rs value </dev/tty
		printf '\n' >/dev/tty

		if [ -n "$pattern" ] && [ -n "$value" ] &&
			! [[ "$value" =~ $pattern ]]; then
			printf '   %s\n' "$complaint" >/dev/tty
			continue
		fi

		# Anything that is not typed back cannot be checked for typos, and a
		# password typo only shows up much later at the Windows login screen.
		if [ -n "$value" ] && [ -z "$pattern" ]; then
			printf '%s' "   Again: " >/dev/tty
			IFS= read -rs again </dev/tty
			printf '\n' >/dev/tty
			if [ "$value" != "$again" ]; then
				printf '   They do not match, try again.\n' >/dev/tty
				continue
			fi
		fi
		break
	done

	# Write to a locked-down temporary file and move it into place, so the
	# secret is never briefly world-readable and an interrupted run leaves no
	# empty file behind — an empty one would read back as "already answered"
	# and the prompt would never appear again.
	local tmp
	tmp=$(mktemp -p "$(dirname "$file")" .secret.XXXXXX)
	chmod 600 "$tmp"
	printf '%s' "$value" >"$tmp"
	mv -f "$tmp" "$file"
}

# Escape the five characters XML cannot carry literally. The product key is
# safe by construction but a password is whatever the user typed, and an
# unescaped & or < turns autounattend.xml into a file Windows Setup silently
# ignores.
xml_escape() {
	sed -e 's/&/\&amp;/g' -e 's/</\&lt;/g' -e 's/>/\&gt;/g' \
		-e 's/"/\&quot;/g' -e "s/'/\&apos;/g"
}

# True if the ISO contains the named path. Reads only the ISO's directory, not
# its contents, so this stays quick on an 8 GB file.
iso_has() {
	7z l -ba "$1" "$2" 2>/dev/null | grep -q .
}

# ---------------------------------------------------------------------------
# Phase 0: check that this machine can actually do it.
# ---------------------------------------------------------------------------

phase_0() {
	say "Phase 0: preflight"

	[ "$(id -u)" -eq 0 ] && die "Run this as your normal user, not root."

	# The script is useless without the files it installs, and copying it
	# somewhere on its own is an easy mistake to make. Say so here rather than
	# three phases later with a half-configured host.
	local missing="" t
	for t in autounattend.xml.in libvirt-qemu-hook.sh extract-vbios.py \
		read-wim-editions.py hugepages-1g.mount domain.xml.in \
		verify-option-rom.py \
		apparmor-libvirt-qemu.local winapp guest-run.py \
		guest-setup.ps1 combine-option-roms.py looking-glass \
		vdd_settings.xml.in install-vdd.ps1 set-display.ps1 \
		run-in-session.ps1; do
		[ -f "$TEMPLATE_DIR/$t" ] || missing="$missing $t"
	done
	[ -z "$missing" ] ||
		die "Missing from $TEMPLATE_DIR:$missing
   These live in the .windows directory next to the script. Copy the whole
   directory, or point TEMPLATE_DIR at where it really is."
	info "Templates found in $TEMPLATE_DIR."

	grep -qE '^flags.*\bsvm\b' /proc/cpuinfo ||
		die "No AMD-V (svm) in /proc/cpuinfo. Enable SVM in the BIOS."
	info "AMD-V available."

	# The IOMMU is what makes passthrough safe and, on most boards, possible at
	# all. If the kernel did not set one up there is nothing in
	# /sys/kernel/iommu_groups to find.
	[ -d /sys/kernel/iommu_groups ] && [ -n "$(ls -A /sys/kernel/iommu_groups)" ] ||
		die "IOMMU is off. Enable IOMMU/SVM in the BIOS, then re-run."
	info "IOMMU on, $(ls /sys/kernel/iommu_groups | wc -l) groups."

	# Both addresses are parsed here rather than where they are used, so a
	# typo in either is reported before anything has been reconfigured.
	pci_split "$IGPU_PCI"
	[ "$PASS_IGPU_AUDIO" = yes ] && pci_split "$IGPU_AUDIO_PCI"

	lspci -n -s "$IGPU_PCI" 2>/dev/null | grep -q "$IGPU_ID" ||
		die "No $IGPU_ID at $IGPU_PCI. Set IGPU_PCI/IGPU_ID for this machine."
	info "iGPU $IGPU_ID found at $IGPU_PCI."

	# The audio ID is checked for the same reason, and it is the one nothing
	# else would catch until after a reboot: it goes onto the kernel command
	# line as vfio-pci.ids, and a wrong value simply leaves the function on
	# its host driver, which phase 4 then refuses to work with.
	if [ "$PASS_IGPU_AUDIO" = yes ]; then
		lspci -n -s "$IGPU_AUDIO_PCI" 2>/dev/null | grep -q "$IGPU_AUDIO_ID" ||
			die "No $IGPU_AUDIO_ID at $IGPU_AUDIO_PCI. Set IGPU_AUDIO_PCI and
   IGPU_AUDIO_ID from 'lspci -Dnn', or set PASS_IGPU_AUDIO=no."
		info "iGPU audio $IGPU_AUDIO_ID found at $IGPU_AUDIO_PCI."
	fi

	# A device can only be passed through together with everything else in its
	# IOMMU group, because the group is the smallest unit the hardware can
	# isolate. Sharing a group with something the host needs means this cannot
	# work without patching ACS support into the kernel.
	local group members
	group=$(basename "$(readlink -f "/sys/bus/pci/devices/$IGPU_PCI/iommu_group")")
	members=$(ls "/sys/kernel/iommu_groups/$group/devices")
	if [ "$(echo "$members" | wc -l)" -gt 1 ]; then
		warn "IOMMU group $group holds more than the iGPU:"
		echo "$members" | sed 's/^/     /'
		warn "Everything listed gets passed through together."
		confirm "Continue anyway?" || exit 1
	else
		info "iGPU is alone in IOMMU group $group."
	fi

	# If the iGPU is the machine's boot display, the kernel's framebuffer
	# console is sitting on it and has to be evicted before vfio can have it.
	# Nothing here does that, so bail with the fix rather than half-working.
	if [ "$(cat "/sys/bus/pci/devices/$IGPU_PCI/boot_vga" 2>/dev/null)" = "1" ]; then
		die "The iGPU is the boot display. Set the BIOS primary display to the
   PCIe slot so the discrete card boots the machine, then re-run."
	fi
	info "iGPU is not the boot display."

	# A monitor plugged into the card being passed through will go dark, which
	# is worth knowing before the reboot rather than after it.
	local card connected=""
	for card in /sys/class/drm/card*/status; do
		[ -e "$card" ] || continue
		# The resolved sysfs path of a connector runs through the PCI device
		# that drives it, so matching on the address identifies its GPU.
		local dev
		dev=$(readlink -f "$(dirname "$card")" 2>/dev/null)
		case "$dev" in
		*"$IGPU_PCI"*)
			[ "$(cat "$card")" = "connected" ] &&
				connected="$connected $(basename "$(dirname "$card")")"
			;;
		esac
	done
	if [ -n "$connected" ]; then
		warn "Displays are connected to the iGPU:$connected"
		warn "They go dark once the guest owns the card."
		confirm "Continue anyway?" || exit 1
	else
		info "No displays connected to the iGPU."
	fi

	# The CPU pinning defaults describe one particular processor, so a machine
	# with a different core count has to be told about it.
	check_cpu_lists

	local ram_total disk_free dir
	ram_total=$(awk '/MemTotal/ {print int($2/1024/1024)}' /proc/meminfo)
	[ "$ram_total" -gt $((VM_RAM_GB + 8)) ] ||
		die "Only ${ram_total}G of RAM for a ${VM_RAM_GB}G guest. Lower VM_RAM_GB."
	info "${ram_total}G RAM total, giving the guest ${VM_RAM_GB}G."

	# The image directory does not exist until libvirt is installed in phase 1,
	# and df on a missing path reports nothing at all. Ask about the nearest
	# parent that is there, which is on the same filesystem anyway.
	dir=$(dirname "$VM_DISK")
	while [ ! -d "$dir" ] && [ "$dir" != / ]; do dir=$(dirname "$dir"); done
	disk_free=$(df -BG --output=avail "$dir" 2>/dev/null |
		tail -1 | tr -dc '0-9')
	if [ -n "$disk_free" ] && [ "$disk_free" -lt "$VM_DISK_GB" ]; then
		warn "Only ${disk_free}G free where the disk image goes, want ${VM_DISK_GB}G."
		warn "The image is sparse, so this may still fit, but it can fill up."
		confirm "Continue anyway?" || exit 1
	fi

	mark_done 0
}

# ---------------------------------------------------------------------------
# Phase 1: host packages.
# ---------------------------------------------------------------------------

phase_1() {
	say "Phase 1: install host packages"

	local core=(
		# QEMU/KVM, libvirt and its GUI.
		qemu-system-x86 qemu-utils libvirt-daemon-system libvirt-clients
		virt-manager virt-viewer dnsmasq-base bridge-utils
		# UEFI firmware for the guest, plus the emulated TPM 2.0 that the
		# Windows 11 installer refuses to go on without.
		ovmf swtpm swtpm-tools
		# Serves the shared folder that puts $HOME inside the guest.
		virtiofsd
		# Builds the answer-file ISO that drives the unattended install.
		xorriso 7zip wget curl jq git
		# Pulls each Windows program's icon out of its .exe and rescales the
		# largest one found: icoutils unpacks, ImageMagick chooses and resizes.
		icoutils imagemagick
	)
	# FreeRDP is what renders each Windows program as its own Linux window.
	local rdp=(freerdp3-x11 libnotify-bin dialog netcat-openbsd iproute2 gawk)
	# The Looking Glass client is not packaged, so it gets compiled.
	local lg_build=(
		cmake gcc g++ pkg-config make binutils-dev fonts-freefont-ttf
		libegl-dev libgl-dev libgles-dev libfontconfig-dev libgmp-dev
		libspice-protocol-dev nettle-dev libxi-dev libxinerama-dev
		libxss-dev libxcursor-dev libxpresent-dev libxkbcommon-dev
		libwayland-dev wayland-protocols libpipewire-0.3-dev libpulse-dev
		libsamplerate0-dev libdecor-0-dev libx11-dev libxfixes-dev
	)

	local want=("${core[@]}" "${rdp[@]}" "${lg_build[@]}") missing=() p
	for p in "${want[@]}"; do
		pkg_installed "$p" || missing+=("$p")
	done

	if [ ${#missing[@]} -eq 0 ]; then
		skip "All ${#want[@]} packages"
	else
		info "Installing ${#missing[@]} of ${#want[@]} packages."
		as_root apt-get update -qq || die "apt-get update failed."
		as_root env DEBIAN_FRONTEND=noninteractive \
			apt-get install -y "${missing[@]}" || die "apt-get install failed."
	fi

	# Managing VMs without root needs membership in both groups. This only
	# takes effect at the next login, which the phase 2 reboot provides.
	local g
	for g in libvirt kvm; do
		if id -nG "$USER" | grep -qw "$g"; then
			skip "group $g"
		else
			info "Adding $USER to group $g."
			as_root usermod -aG "$g" "$USER"
		fi
	done

	as_root systemctl enable --now libvirtd.socket virtlogd.socket >/dev/null 2>&1

	# The NAT network the guest gets its address on. Without this the VM has no
	# network at all, and RDP to it is how WinApps works.
	# Run these as root rather than as the user: the libvirt group membership
	# added above does not apply until the next login, and until it does every
	# virsh call against qemu:///system would raise a polkit prompt.
	if as_root virsh net-info default >/dev/null 2>&1; then
		as_root virsh net-autostart default >/dev/null 2>&1
		as_root virsh net-info default 2>/dev/null |
			grep -q 'Active:.*yes' ||
			as_root virsh net-start default >/dev/null 2>&1
		info "libvirt 'default' network is up."
	else
		warn "libvirt has no 'default' network; the guest may have no network."
	fi

	mark_done 1
}

# ---------------------------------------------------------------------------
# Phase 2: hand the iGPU to vfio-pci and set up the host for passthrough.
# Needs a reboot at the end.
# ---------------------------------------------------------------------------

# Pull the iGPU's VBIOS out of the ACPI VFCT table.
#
# A discrete card exposes its VBIOS through a PCI ROM BAR that QEMU can just
# read. An integrated one has no ROM BAR at all: the firmware carries the image
# in an ACPI table named VFCT instead, which is where the host amdgpu driver
# gets it from too. Without handing that image to QEMU as a romfile, the guest
# driver has no video BIOS to run and the card never initialises.
#
# The table is an ACPI header (36 bytes), a 16-byte UUID, and then a u32 offset
# to a chain of images. Each image is a 28-byte header naming the PCI address
# and PCI IDs it belongs to, followed by that many bytes of ROM. Walk the chain
# and take the one matching this GPU.
# True if the file is a PCI option ROM containing an image of the given code
# type (0 for a legacy x86 BIOS, 3 for a UEFI driver). Prints what it found.
verify_option_rom() {
	# Unquoted expansion on purpose: with no code type asked for, the
	# argument has to vanish rather than arrive as an empty string.
	python3 "$TEMPLATE_DIR/verify-option-rom.py" "$1" ${2:+"$2"}
}

extract_vbios() {
	local out=$1 table=/sys/firmware/acpi/tables/VFCT tmp

	[ -r "$table" ] || {
		# The table is root-only, so copy it out first.
		tmp=$(mktemp)
		as_root cp "$table" "$tmp" 2>/dev/null || return 1
		as_root chmod 644 "$tmp"
		table=$tmp
	}

	python3 "$TEMPLATE_DIR/extract-vbios.py" \
		"$table" "$out" "$IGPU_PCI" "$IGPU_ID"
	local rc=$?
	[ -n "$tmp" ] && rm -f "$tmp"
	return $rc
}

# The current value of a /etc/default/grub setting, with its quotes stripped.
# Empty both when the setting is unset and when it is set to nothing, which is
# the same thing as far as anything here is concerned.
grub_get() {
	local raw
	raw=$(grep -E "^$1=" /etc/default/grub 2>/dev/null | tail -1)
	raw=${raw#*=}
	raw=${raw#[\"\']}
	printf '%s' "${raw%[\"\']}"
}

# Set one /etc/default/grub setting, appending the line if the file has no such
# setting to rewrite. Returns 0 only when the file changed, so callers can skip
# the slow update-grub when a re-run changes nothing.
grub_set() {
	local key=$1 value=$2 file=/etc/default/grub
	[ "$(grub_get "$key")" = "$value" ] && return 1
	if grep -qE "^${key}=" "$file"; then
		as_root sed -i -E "s|^${key}=.*|${key}=\"${value}\"|" "$file"
	else
		# Ubuntu ships GRUB_TIMEOUT_STYLE commented out, and a sed that matches
		# nothing would leave the setting unapplied while reporting success.
		printf '%s="%s"\n' "$key" "$value" | as_root tee -a "$file" >/dev/null
	fi
	info "$key is now \"$value\""
	return 0
}

# Rewrite one GRUB_CMDLINE_* setting so it contains exactly the arguments we
# want, leaving anything else on the line alone. Returns 0 if the line changed.
set_kernel_args() {
	local key=$1 current new arg name
	shift

	current=$(grub_get "$key")
	new=$current
	for arg in "$@"; do
		# Replace any existing setting of the same key=, so re-running with a
		# different value corrects it instead of appending a second copy.
		name=${arg%%=*}
		new=$(sed -E "s/(^| )${name}=[^ ]*//g" <<<"$new")
		new="$new $arg"
	done
	# Squash the whitespace the substitutions leave behind.
	new=$(tr -s ' ' <<<"$new" | sed -E 's/^ | $//g')

	grub_set "$key" "$new"
}

phase_2() {
	say "Phase 2: configure the host for passthrough"

	# --only 2 skips phase 0. extract-vbios.py picks the VBIOS out of the VFCT
	# table by matching this address, so it is validated here too rather than
	# left to fail as a Python traceback halfway through.
	pci_split "$IGPU_PCI"

	local need_initramfs=0 need_grub=0

	# --- The VBIOS -------------------------------------------------------
	if as_root test -s "$VBIOS"; then
		skip "VBIOS at $VBIOS"
	else
		info "Extracting the iGPU VBIOS from the ACPI VFCT table."
		local tmp
		tmp=$(mktemp)
		extract_vbios "$tmp" || die "Could not extract the VBIOS."
		as_root mkdir -p "$VBIOS_DIR"
		as_root install -m 644 "$tmp" "$VBIOS"
		rm -f "$tmp"
		info "Wrote $VBIOS"
	fi

	# --- The UEFI GOP driver ---------------------------------------------
	# The VBIOS above is a legacy x86 image, which UEFI firmware cannot use to
	# put a picture on the card. Without a Graphics Output Protocol driver the
	# guest brings the device up and then stops with error code 43. See
	# GOP_ROM_URL for where this one comes from and what trusting it means.
	if as_root test -s "$GOP_ROM"; then
		skip "UEFI GOP driver at $GOP_ROM"
	else
		info "Fetching the UEFI GOP driver."
		local gop
		gop=$(mktemp)
		if curl -fL --proto '=https' --proto-redir '=https' --retry 3 -o "$gop" "$GOP_ROM_URL" 2>/dev/null &&
			verify_option_rom "$gop" 3; then
			as_root mkdir -p "$VBIOS_DIR"
			as_root install -m 644 "$gop" "$GOP_ROM"
			info "Wrote $GOP_ROM"
		else
			warn "Could not fetch a usable GOP driver from $GOP_ROM_URL."
			warn "The guest will start, but the iGPU will show error code 43."
		fi
		rm -f "$gop"
	fi

	# --- The chained ROM the guest is handed ------------------------------
	# Rebuilt whenever either half is newer than it, so that replacing the GOP
	# driver is a matter of dropping in a new file and re-running this phase.
	if ! as_root test -s "$GOP_ROM"; then
		skip "chained ROM (no GOP driver to chain)"
	elif as_root test "$COMBINED_ROM" -nt "$VBIOS" &&
		as_root test "$COMBINED_ROM" -nt "$GOP_ROM"; then
		skip "chained ROM at $COMBINED_ROM"
	else
		info "Chaining the GOP driver behind the VBIOS."
		local combined
		combined=$(mktemp)
		# Both images are rewritten to carry the graphics function's own IDs.
		# The GOP driver arrives claiming whichever card it was salvaged from,
		# and firmware that checks would pass over it.
		if python3 "$TEMPLATE_DIR/combine-option-roms.py" \
			"$combined" "$IGPU_ID" "$VBIOS" "$GOP_ROM" &&
			verify_option_rom "$combined" 3 &&
			verify_option_rom "$combined" 0 >/dev/null; then
			as_root install -m 644 "$combined" "$COMBINED_ROM"
			info "Wrote $COMBINED_ROM"
		else
			warn "Could not chain the ROMs; falling back to the bare VBIOS."
			as_root rm -f "$COMBINED_ROM"
		fi
		rm -f "$combined"
	fi

	# --- Bind the iGPU to vfio-pci ---------------------------------------
	# Two things have to line up here. vfio-pci must be told which device to
	# claim, and it must be loaded before amdgpu, because whichever driver
	# binds first keeps the card. The softdep enforces the ordering and the
	# initramfs entry makes sure both are available early enough to matter.
	# Built into a variable rather than fed in from a heredoc, the way the
	# hook and the AppArmor rules below are. A heredoc that is the redirection
	# for an if's condition puts its body after the "then", which is correct
	# but reads badly and defeats every syntax highlighter tried on it.
	local vfio_conf
	vfio_conf=$(printf '%s\n' \
		'# Hand the integrated GPU to vfio-pci for PCI passthrough.' \
		'# Written by make_windows_vm.sh.' \
		"options vfio-pci ids=${vfio_ids} disable_vga=1" \
		'softdep amdgpu pre: vfio-pci')

	if write_root_file /etc/modprobe.d/vfio-igpu.conf <<<"$vfio_conf"; then
		info "Wrote /etc/modprobe.d/vfio-igpu.conf"
		need_initramfs=1
	else
		skip "/etc/modprobe.d/vfio-igpu.conf"
	fi

	# amdgpu is not in this machine's initramfs today, so the modprobe softdep
	# above is enough on its own. Putting vfio in the initramfs anyway costs
	# nothing and covers the case where a kernel update starts shipping amdgpu
	# early: whichever driver reaches the card first keeps it, and losing that
	# race means booting to a host that has quietly taken the GPU back.
	if append_root_block /etc/initramfs-tools/modules \
		"# --- make_windows_vm.sh: vfio for iGPU passthrough ---" <<-'EOF'; then
			vfio
			vfio_iommu_type1
			vfio_pci
		EOF
		info "Added vfio modules to /etc/initramfs-tools/modules"
		need_initramfs=1
	else
		skip "initramfs vfio modules"
	fi

	# --- Kernel command line ---------------------------------------------
	if set_kernel_args GRUB_CMDLINE_LINUX_DEFAULT "${kernel_args[@]}"; then
		need_grub=1
	else
		skip "kernel command line"
	fi

	# Make the boot menu reachable. If the kernel command line ever leaves the
	# machine unbootable, editing vfio-pci.ids back out at the GRUB prompt is
	# the way back, and a hidden zero-second menu makes that a real struggle.
	if [ "$GRUB_RECOVERY_TIMEOUT" -gt 0 ]; then
		local changed=0
		grub_set GRUB_TIMEOUT "$GRUB_RECOVERY_TIMEOUT" && changed=1
		grub_set GRUB_TIMEOUT_STYLE menu && changed=1
		if [ "$changed" = 1 ]; then
			info "The GRUB menu shows for ${GRUB_RECOVERY_TIMEOUT}s, so the boot line stays editable."
			need_grub=1
		else
			skip "GRUB recovery menu"
		fi
	fi

	# --- Hugepages, allocated only while the VM runs ----------------------
	# Reserving hugepages on the kernel command line takes them away from Linux
	# for the whole uptime, whether or not the VM is running. libvirt runs this
	# hook around a domain's lifecycle instead, so the memory comes back the
	# moment Windows shuts down.
	#
	# 1 GiB pages need a gigabyte of physically contiguous memory each, which
	# can simply not be available once memory is fragmented. Rather than let
	# that turn into a VM that refuses to start, the hook falls back to 2 MiB
	# pages, and then to no hugepages at all, rewriting the domain XML on the
	# way past. libvirt lets the prepare hook do that by printing the new XML.
	local hook
	hook=$(render_template libvirt-qemu-hook.sh \
		VM_NAME "$VM_NAME" VM_RAM_GB "$VM_RAM_GB") ||
		die "Could not build the libvirt hugepage hook."

	local need_libvirtd=0
	if write_root_file /etc/libvirt/hooks/qemu 755 <<<"$hook"; then
		info "Wrote /etc/libvirt/hooks/qemu (dynamic hugepages)"
		need_libvirtd=1
	else
		skip "libvirt hugepage hook"
	fi

	# --- Somewhere for the 1 GiB pages to live ---------------------------
	# Ubuntu mounts hugetlbfs only for 2 MiB pages. libvirt picks the mount
	# whose page size matches what the domain asks for, so without a 1 GiB
	# mount the guest never starts:
	#
	#   Unable to find any usable hugetlbfs mount for 1048576 KiB
	#
	# The pages themselves are still allocated on demand by the hook above;
	# this only gives them a filesystem to be handed out through.
	local mount_unit
	mount_unit=$(render_template hugepages-1g.mount VM_NAME "$VM_NAME") ||
		die "Could not build the hugepage mount unit."

	if write_root_file /etc/systemd/system/dev-hugepages1G.mount <<<"$mount_unit"; then
		info "Wrote /etc/systemd/system/dev-hugepages1G.mount"
		as_root systemctl daemon-reload
		as_root systemctl enable --now dev-hugepages1G.mount >/dev/null 2>&1 ||
			warn "Could not mount /dev/hugepages1G; 1 GiB pages will not work."
		need_libvirtd=1
	else
		skip "1 GiB hugepage mount"
		# The unit can be written but not running, after a failed enable or
		# a hand-edit, and the symptom is a VM that will not start.
		systemctl is-active --quiet dev-hugepages1G.mount ||
			as_root systemctl start dev-hugepages1G.mount >/dev/null 2>&1
	fi

	# --- Let AppArmor see the files QEMU needs ---------------------------
	# The per-domain AppArmor profile is generated from the domain XML by
	# virt-aa-helper, which does not notice <rom file='...'/>. The result is a
	# guest that refuses to start with "failed to find romfile" pointing at a
	# file that plainly exists and is readable. This adds the two paths it
	# cannot infer.
	local aa_local=/etc/apparmor.d/local/abstractions/libvirt-qemu
	local aa_rules
	aa_rules=$(render_template apparmor-libvirt-qemu.local \
		VBIOS_DIR "$VBIOS_DIR" LG_SHM_NAME "$LG_SHM_NAME") ||
		die "Could not build the AppArmor rules."

	if [ ! -d /etc/apparmor.d/abstractions ]; then
		skip "AppArmor rules (AppArmor is not installed)"
	elif write_root_file "$aa_local" <<<"$aa_rules"; then
		info "Wrote $aa_local"
		# Profiles compile the abstraction in at parse time, so the ones
		# already loaded have to be rebuilt before this takes effect.
		as_root systemctl reload apparmor >/dev/null 2>&1 ||
			as_root apparmor_parser -r /etc/apparmor.d/usr.sbin.libvirtd >/dev/null 2>&1 ||
			warn "Could not reload AppArmor; a reboot will apply the rules."
		need_libvirtd=1
	else
		skip "AppArmor rules"
	fi

	# --- Shared memory for Looking Glass ---------------------------------
	# QEMU and the Looking Glass client both open this file, and they run as
	# different users. Creating it up front with group kvm, which libvirt-qemu
	# is a member of, lets both reach it without loosening anything else.
	local lg_tmpfile
	lg_tmpfile=$(printf '%s\n' \
		'#Type Path Mode UID GID Age Argument' \
		"f /dev/shm/${LG_SHM_NAME} 0660 ${USER} kvm -")

	if write_root_file /etc/tmpfiles.d/10-looking-glass.conf <<<"$lg_tmpfile"; then
		info "Wrote /etc/tmpfiles.d/10-looking-glass.conf"
		as_root systemd-tmpfiles --create /etc/tmpfiles.d/10-looking-glass.conf
	else
		skip "Looking Glass shared memory rule"
	fi

	# Restart libvirt once, after everything it reads at startup is in place:
	# the hugetlbfs mount (it scans the mount table only then) and the hook.
	if [ "$need_libvirtd" = 1 ]; then
		info "Restarting libvirtd to pick up the new configuration."
		as_root systemctl restart libvirtd >/dev/null 2>&1
	fi

	# --- Regenerate boot artefacts ---------------------------------------
	if [ "$need_initramfs" = 1 ]; then
		info "Rebuilding the initramfs."
		as_root update-initramfs -u -k all || die "update-initramfs failed."
	fi
	if [ "$need_grub" = 1 ]; then
		info "Regenerating the GRUB config."
		as_root update-grub || die "update-grub failed."
	fi

	mark_done 2

	if [ "$(basename "$(readlink -f "/sys/bus/pci/devices/$IGPU_PCI/driver" 2>/dev/null)" 2>/dev/null)" = "vfio-pci" ]; then
		info "The iGPU is already on vfio-pci; no reboot needed."
	else
		say "Reboot required"
		info "The iGPU is still bound to amdgpu. Reboot to hand it to vfio-pci,"
		info "then run this script again to carry on with phase 3."
	fi
}

# ---------------------------------------------------------------------------
# Phase 3: installation media.
# ---------------------------------------------------------------------------

# Find a Windows 11 installation ISO. WIN_ISO wins if it is set; otherwise this
# looks where a browser drops downloads. A file only counts if it carries both
# the setup program and an install image, which is what separates real
# installation media from the recovery, ARM and update ISOs that look the same
# from the filename alone.
find_windows_iso() {
	local candidate

	# Runs inside a command substitution, so it reports failure by returning
	# rather than calling die: an exit here would only end the subshell.
	if [ -n "$WIN_ISO" ]; then
		[ -f "$WIN_ISO" ] || return 2
		echo "$WIN_ISO"
		return 0
	fi

	for candidate in \
		"$HOME/Downloads"/*.iso "$HOME/Downloads"/*.ISO \
		"$HOME"/*.iso "$HOME/Desktop"/*.iso \
		/var/lib/libvirt/images/*.iso; do
		[ -f "$candidate" ] || continue
		# The virtio driver ISO lives in the same directory once phase 3 has
		# run once, and it is emphatically not a Windows installer.
		case "$(basename "$candidate")" in virtio-win*) continue ;; esac
		iso_has "$candidate" "setup.exe" || continue
		if iso_has "$candidate" "sources/install.wim" ||
			iso_has "$candidate" "sources/install.esd"; then
			echo "$candidate"
			return 0
		fi
	done

	return 1
}

# List the Windows editions inside an ISO, one per line. An install image
# carries its catalogue as a UTF-16 XML blob whose location is recorded in the
# WIM header, so this reads two small pieces of a 7 GB file rather than
# unpacking any of it.
#
# The ISO is mounted through udisks, which lets a logged-in user mount an image
# without becoming root. That matters: this check is a convenience, and making
# it ask for a password would cost more than it is worth. If udisks is not
# there or refuses, the caller carries on unchecked rather than escalating.
windows_iso_editions() {
	local iso=$1 loop="" mnt="" wim

	have udisksctl || return 1

	loop=$(udisksctl loop-setup -r -f "$iso" 2>/dev/null |
		grep -oE '/dev/loop[0-9]+' | head -1)
	[ -n "$loop" ] || return 1

	# From here on every exit has to give the loop device back, or the ISO
	# stays attached and shows up in the file manager as a mounted disc.
	udisksctl mount -b "$loop" >/dev/null 2>&1
	mnt=$(findmnt -no TARGET "$loop" 2>/dev/null | head -1)

	if [ -n "$mnt" ]; then
		for wim in "$mnt/sources/install.wim" "$mnt/sources/install.esd"; do
			[ -f "$wim" ] || continue
			python3 "$TEMPLATE_DIR/read-wim-editions.py" "$wim" 2>/dev/null
			break
		done
		udisksctl unmount -b "$loop" >/dev/null 2>&1
	fi

	udisksctl loop-delete -b "$loop" >/dev/null 2>&1
	[ -n "$mnt" ]
}

phase_3() {
	say "Phase 3: installation media"

	mkdir -p "$state_dir"
	chmod 700 "$state_dir"

	# --- The Windows installation ISO ------------------------------------
	local iso rc=0
	iso=$(find_windows_iso) || rc=$?
	[ "$rc" -eq 2 ] &&
		die "WIN_ISO is set to '$WIN_ISO', which is not a file."
	if [ "$rc" -ne 0 ]; then
		die "No Windows 11 installation ISO found.

   Microsoft serves these from a page that rejects scripted downloads, so
   this is the one file you have to fetch by hand:

       https://www.microsoft.com/software-download/windows11

   Pick 'Windows 11 (multi-edition ISO for x64 devices)', your language,
   then the 64-bit download. Save it in ~/Downloads and run this again, or
   point at it directly:

       WIN_ISO=/path/to/Win11.iso $0 --from 3"
	fi
	info "Windows ISO: $iso ($(du -h "$iso" | cut -f1))"

	# Catching the wrong edition name here saves discovering it a long way
	# into an unattended install that then stops to ask which one to use.
	local editions
	if editions=$(windows_iso_editions "$iso") && [ -n "$editions" ]; then
		if grep -qxF "$WIN_EDITION" <<<"$editions"; then
			info "Edition '$WIN_EDITION' is present in the ISO."
		else
			warn "'$WIN_EDITION' is not in this ISO. It offers:"
			sed 's/^/     /' <<<"$editions" >&2
			die "Set WIN_EDITION to one of those and re-run."
		fi
		# Home has no RDP server at all, so the seamless-window half of this
		# setup cannot work on it. Better to say so now than at phase 5.
		case "$WIN_EDITION" in
		*Home*)
			warn "$WIN_EDITION has no RDP server, so individual Windows"
			warn "programs cannot appear as Linux windows. Looking Glass"
			warn "will still give you the full desktop."
			confirm "Continue with $WIN_EDITION anyway?" || exit 1
			;;
		esac
	else
		warn "Could not read the edition list from the ISO; carrying on."
	fi

	# --- Secrets ----------------------------------------------------------
	ask_secret "$key_file" \
		"   Windows product key (leave empty to install unactivated): " \
		'^[A-Za-z0-9]{5}(-[A-Za-z0-9]{5}){4}$' \
		"That is not a 25-character XXXXX-XXXXX-XXXXX-XXXXX-XXXXX key."

	ask_secret "$pw_file" \
		"   Password for the Windows account '$WIN_USER': "

	local product_key windows_pw
	product_key=$(cat "$key_file")
	windows_pw=$(cat "$pw_file")

	# RDP refuses to log in an account with no password, and RDP is how
	# individual Windows programs reach your Linux desktop later.
	[ -n "$windows_pw" ] ||
		die "The Windows account needs a password: RDP will not log in without
   one, and that is what phase 5 uses. Delete $pw_file and re-run."

	if [ -n "$product_key" ]; then
		# A key written into the file by hand has never been through the
		# prompt's format check, and a mistyped one is only discovered when
		# Setup stops halfway through to ask for a good one.
		if ! [[ "$product_key" =~ ^[A-Za-z0-9]{5}(-[A-Za-z0-9]{5}){4}$ ]]; then
			warn "The key in $key_file is not in XXXXX-XXXXX-XXXXX-XXXXX-XXXXX form."
			confirm "Use it anyway?" || die "Fix or delete $key_file and re-run."
		fi
		info "Product key supplied ($(wc -c <"$key_file") bytes, not shown)."
	else
		info "No product key: Windows installs unactivated, which runs"
		info "indefinitely with a watermark. A key can be added later."
	fi

	# --- The virtio driver ISO -------------------------------------------
	# Windows Setup has no driver for a virtio disk, so without this it gets
	# as far as "where do you want to install Windows?" and shows nothing.
	if [ -f "$VIRTIO_ISO" ] && iso_has "$VIRTIO_ISO" "viostor/w11/amd64/viostor.inf"; then
		skip "virtio driver ISO"
	else
		info "Fetching the virtio driver ISO."
		local tmp
		tmp=$(mktemp -p /var/tmp virtio-win.XXXXXX.iso)
		curl -fL --proto '=https' --proto-redir '=https' \
			--progress-bar --retry 3 --retry-delay 2 \
			-o "$tmp" "$VIRTIO_URL" ||
			{
				rm -f "$tmp"
				die "Could not download $VIRTIO_URL"
			}
		# Fedora publishes checksums for the RPMs but not for this ISO, so
		# the check that it arrived intact is that it contains the driver
		# the installation actually depends on.
		iso_has "$tmp" "viostor/w11/amd64/viostor.inf" ||
			{
				rm -f "$tmp"
				die "The downloaded virtio ISO has no Windows 11 storage driver in it."
			}
		as_root install -m 644 "$tmp" "$VIRTIO_ISO"
		rm -f "$tmp"
		info "Installed $VIRTIO_ISO"
	fi

	build_unattend_iso "$product_key" "$windows_pw"

	mark_done 3
}

# Build the ISO that drives the Windows installation. Windows Setup looks for
# autounattend.xml in the root of every drive it can see, so this small ISO
# rides along as a second CD-ROM and answers every question the installer would
# otherwise stop to ask.
#
# It contains the product key and the account password in clear text, which is
# unavoidable — that is the format Setup reads — so it is written root-owned
# and not world-readable, and phase 4 detaches it once Windows is installed.
build_unattend_iso() {
	local product_key=$1 windows_pw=$2
	local dir key_block pw_xml
	dir=$(mktemp -d)

	pw_xml=$(printf '%s' "$windows_pw" | xml_escape)

	# An empty <ProductKey> is not the same as no key: it makes Setup stop and
	# ask. Leave the element out entirely to install unactivated.
	key_block=""
	if [ -n "$product_key" ]; then
		key_block="            <ProductKey>
                <Key>$(printf '%s' "$product_key" | xml_escape)</Key>
                <WillShowUI>OnError</WillShowUI>
            </ProductKey>"
	fi

	# Setup assigns CD-ROM drive letters in an order that is not worth relying
	# on, so every plausible letter is offered for the driver search. Paths
	# that do not exist are skipped rather than treated as an error.
	# WinPE hands out letters from C: upwards to whatever it can see, and with
	# no disk visible yet that is the CD-ROMs, in an order not worth relying
	# on. Offering every plausible letter costs nothing: the component this
	# goes into skips paths that do not exist.
	local driver_paths="" letter
	for letter in C D E F G H; do
		driver_paths="$driver_paths
                <PathAndCredentials wcm:action=\"add\" wcm:keyValue=\"${letter}1\">
                    <Path>${letter}:\\viostor\\w11\\amd64</Path>
                </PathAndCredentials>
                <PathAndCredentials wcm:action=\"add\" wcm:keyValue=\"${letter}2\">
                    <Path>${letter}:\\NetKVM\\w11\\amd64</Path>
                </PathAndCredentials>"
	done

	# Everything that lands in element content is escaped, not just the
	# password. These are all settings, and an & in any of them — a username,
	# an edition name — makes the answer file malformed, which Setup responds
	# to by ignoring it and installing interactively.
	render_template autounattend.xml.in \
		LOCALE "$(printf '%s' "$WIN_LOCALE" | xml_escape)" \
		EDITION "$(printf '%s' "$WIN_EDITION" | xml_escape)" \
		DRIVER_PATHS "$driver_paths" \
		PRODUCT_KEY_BLOCK "$key_block" \
		HOSTNAME "$(printf '%s' "$WIN_HOSTNAME" | xml_escape)" \
		TIMEZONE "$(printf '%s' "$WIN_TIMEZONE" | xml_escape)" \
		USERNAME "$(printf '%s' "$WIN_USER" | xml_escape)" \
		PASSWORD "$pw_xml" \
		>"$dir/autounattend.xml" || {
		rm -rf "$dir"
		die "Could not build autounattend.xml."
	}

	# A malformed answer file is ignored in silence: Setup just runs
	# interactively and there is nothing to say why. Worth checking.
	if have python3; then
		python3 -c 'import sys,xml.dom.minidom as m; m.parse(sys.argv[1])' \
			"$dir/autounattend.xml" ||
			{
				rm -rf "$dir"
				die "Generated autounattend.xml is not well-formed XML."
			}
	fi

	local tmp
	tmp=$(mktemp -p /var/tmp unattend.XXXXXX.iso)
	xorriso -as mkisofs -quiet -J -r -V UNATTEND -o "$tmp" "$dir" ||
		{
			rm -rf "$dir" "$tmp"
			die "xorriso could not build the answer file ISO."
		}

	# Group kvm, not world: qemu runs as a member of that group and has to
	# read this, but it holds a product key and a password in clear text.
	as_root install -m 640 -g kvm "$tmp" "$UNATTEND_ISO"
	rm -rf "$dir" "$tmp"
	info "Built $UNATTEND_ISO"
}

# ---------------------------------------------------------------------------
# Phase 4: define the VM and install Windows.
# ---------------------------------------------------------------------------

# Build the <vcpupin> lines that tie each guest thread to a host thread.
#
# The pairing is what matters. VM_CORES and VM_SIBLINGS are two halves of the
# same physical cores, so taking one from each in step puts guest core N's two
# threads on host threads that really are siblings. Interleave them wrongly and
# Windows schedules two busy threads onto one physical core while another sits
# idle.
build_vcpupin() {
	local -a cores siblings
	IFS=',' read -ra cores <<<"$VM_CORES"
	IFS=',' read -ra siblings <<<"$VM_SIBLINGS"

	local i
	for i in "${!cores[@]}"; do
		printf "    <vcpupin vcpu='%d' cpuset='%s'/>\n" \
			"$((i * 2))" "${cores[i]}"
		printf "    <vcpupin vcpu='%d' cpuset='%s'/>\n" \
			"$((i * 2 + 1))" "${siblings[i]}"
	done
}

# Warn if the chosen host threads are not what the machine actually calls
# siblings. Getting this wrong costs performance silently, and the kernel
# already knows the answer.
check_topology() {
	local -a cores siblings
	IFS=',' read -ra cores <<<"$VM_CORES"
	IFS=',' read -ra siblings <<<"$VM_SIBLINGS"

	local i want got bad=0
	for i in "${!cores[@]}"; do
		want="${cores[i]},${siblings[i]}"
		got=$(cat "/sys/devices/system/cpu/cpu${cores[i]}/topology/thread_siblings_list" 2>/dev/null)
		[ "$got" = "$want" ] || {
			warn "cpu${cores[i]}'s siblings are '$got', but the pairing says '$want'."
			bad=1
		}
	done

	[ "$bad" = 0 ] &&
		info "vCPU pinning matches the host's SMT layout."
}

# Get past "Press any key to boot from CD or DVD".
#
# Windows installation media prints that, waits about five seconds, and gives
# up if nothing arrives — which is always, during an unattended install with
# nobody watching. The ISO does carry a no-prompt boot image
# (efi/microsoft/boot/efisys_noprompt.bin), but Windows stores boot images as
# hidden El Torito entries rather than as files in the filesystem, so putting
# it to use means unpacking and rebuilding the whole 8 GB image. Sending the
# key is cheaper and just as reliable.
#
# Keys that arrive after the prompt has gone land in Setup, which is answering
# from autounattend.xml and asks nothing, so overshooting costs nothing.
press_any_key() {
	local deadline=$((SECONDS + 45))
	info "Sending keypresses to get past the boot prompt."
	while [ "$SECONDS" -lt "$deadline" ]; do
		$VIRSH send-key "$VM_NAME" \
			--codeset linux KEY_ENTER >/dev/null 2>&1 || break
		sleep 0.5
	done
}

# The iGPU's audio function, when it is being passed through.
#
# This is the sound that travels down an HDMI or DisplayPort cable, so it
# belongs to the guest for the same reason the picture does. It also shares an
# IOMMU group with the graphics function, and a group is the smallest unit that
# can be passed through, so the two go together in any case.
#
# No option ROM here. The GOP driver goes on the graphics function, chained
# behind its VBIOS; see COMBINED_ROM.
build_audio_hostdev() {
	[ "$PASS_IGPU_AUDIO" = yes ] || return 0

	pci_split "$IGPU_AUDIO_PCI"

	cat <<-EOF

		    <!-- The iGPU's audio function: HDMI and DisplayPort sound. -->
		    <hostdev mode='subsystem' type='pci' managed='no'>
		      <source>
		        <address domain='$pci_domain' bus='$pci_bus' slot='$pci_slot' function='$pci_func'/>
		      </source>
		    </hostdev>
	EOF
}

# The ROM file to hand the graphics function.
#
# The chained one when phase 2 managed to build it, and the bare VBIOS when it
# did not, so that a missing GOP driver costs the display acceleration rather
# than the whole VM.
igpu_rom() {
	if as_root test -s "$COMBINED_ROM"; then
		printf '%s' "$COMBINED_ROM"
	else
		printf '%s' "$VBIOS"
	fi
}

# The CD-ROM devices for the domain.
#
# While installing, the Windows and answer-file discs are attached and the
# installer leads the boot order. Once Windows is installed they are left out
# entirely: the answer file repartitions disk 0 without prompting, so an
# attached installer disc is a loaded gun pointed at a working system. The
# virtio disc stays either way, because it is where the guest driver installers
# live.
build_install_media() {
	local installing=$1

	# The one path here that is not ours: the ISO is picked up from wherever
	# the browser dropped it, so its name can hold characters XML cannot,
	# like the & in "Win11 & drivers.iso".
	local win_iso_xml
	win_iso_xml=$(printf '%s' "$WIN_ISO_PATH" | xml_escape)

	if [ "$installing" = yes ]; then
		cat <<-EOF
			    <disk type='file' device='cdrom'>
			      <driver name='qemu' type='raw'/>
			      <source file='$win_iso_xml'/>
			      <target dev='sda' bus='sata'/>
			      <readonly/>
			      <boot order='1'/>
			    </disk>

			    <!-- Setup cannot see a virtio disk without the driver here. -->
			    <disk type='file' device='cdrom'>
			      <driver name='qemu' type='raw'/>
			      <source file='$VIRTIO_ISO'/>
			      <target dev='sdb' bus='sata'/>
			      <readonly/>
			    </disk>

			    <!-- autounattend.xml, holding the product key and the account
			         password in clear text. Dropped once Windows is in. -->
			    <disk type='file' device='cdrom'>
			      <driver name='qemu' type='raw'/>
			      <source file='$UNATTEND_ISO'/>
			      <target dev='sdc' bus='sata'/>
			      <readonly/>
			    </disk>
		EOF
	else
		cat <<-EOF
			    <!-- The guest driver installers. The installation media is
			         deliberately absent: see build_install_media. -->
			    <disk type='file' device='cdrom'>
			      <driver name='qemu' type='raw'/>
			      <source file='$VIRTIO_ISO'/>
			      <target dev='sdb' bus='sata'/>
			      <readonly/>
			    </disk>
		EOF
	fi
}

phase_4() {
	say "Phase 4: define the VM"

	phase_done 3 || die "Phase 3 has not run: there is no installation media yet."

	# --- The system disk ---------------------------------------------------
	if [ -f "$VM_DISK" ]; then
		skip "disk image $VM_DISK"
	else
		info "Creating a ${VM_DISK_GB}G sparse image at $VM_DISK."
		# Sparse: the file only takes the space the guest has actually
		# written, so the size above is a ceiling rather than a reservation.
		as_root qemu-img create -f raw -o preallocation=off \
			"$VM_DISK" "${VM_DISK_GB}G" >/dev/null ||
			die "Could not create $VM_DISK"
		as_root chown root:kvm "$VM_DISK"
		as_root chmod 660 "$VM_DISK"
	fi

	# --- Installing, or redefining an existing machine? --------------------
	# This decides whether the installation media is attached at all, and it
	# is the difference between adjusting a setting and destroying the guest.
	local installing=yes
	if [ -f "$installed_marker" ]; then
		installing=no
		info "Windows is already installed; leaving the installation media off."
	fi

	# --- Sanity-check the pieces the domain points at ----------------------
	local win_iso=""
	if [ "$installing" = yes ]; then
		win_iso=$(find_windows_iso) ||
			die "The Windows ISO has gone missing since phase 3."
		[ -f "$UNATTEND_ISO" ] || die "Missing $UNATTEND_ISO; re-run phase 3."
	fi
	[ -f "$VIRTIO_ISO" ] || die "Missing $VIRTIO_ISO; re-run phase 3."
	as_root test -f "$VBIOS" || die "Missing $VBIOS; re-run phase 2."

	# The guest cannot start if something else grabbed the card back.
	local bound
	bound=$(basename "$(readlink -f "/sys/bus/pci/devices/$IGPU_PCI/driver" 2>/dev/null)" 2>/dev/null)
	[ "$bound" = "vfio-pci" ] ||
		die "The iGPU is bound to '${bound:-nothing}', not vfio-pci. Re-run phase 2 and reboot."
	info "iGPU $IGPU_PCI is on vfio-pci."

	# The audio function has to be claimed too, and it is claimed at boot, so
	# a mismatch here means phase 2 has run but the reboot has not.
	if [ "$PASS_IGPU_AUDIO" = yes ]; then
		bound=$(basename "$(readlink -f "/sys/bus/pci/devices/$IGPU_AUDIO_PCI/driver" 2>/dev/null)" 2>/dev/null)
		if [ "$bound" = "vfio-pci" ]; then
			info "iGPU audio $IGPU_AUDIO_PCI is on vfio-pci."
		else
			die "The iGPU audio function is bound to '${bound:-nothing}', not
   vfio-pci. Run phase 2 and reboot, or set PASS_IGPU_AUDIO=no."
		fi
	fi

	check_topology

	# --- The shared directory ----------------------------------------------
	# virtiofsd refuses to start if this is missing, which shows up as a VM
	# that will not boot rather than as a missing drive.
	if [ -d "$VM_SHARE_DIR" ]; then
		skip "shared directory $VM_SHARE_DIR"
	else
		mkdir -p "$VM_SHARE_DIR" ||
			die "Could not create $VM_SHARE_DIR"
		info "Created $VM_SHARE_DIR, shared into Windows as drive Z:."
	fi

	# --- Render the domain -------------------------------------------------
	# --only 4 skips phase 0, so the settings the XML is built from are
	# checked here too rather than trusted.
	check_cpu_lists

	local -a cores
	IFS=',' read -ra cores <<<"$VM_CORES"
	local ncores=${#cores[@]}

	# Both addresses are split here, in this shell. build_audio_hostdev splits
	# its own again inside a command substitution, where a die would end only
	# the subshell and leave the audio function quietly out of the domain.
	# Graphics goes last so its fields are the ones left in the variables.
	[ "$PASS_IGPU_AUDIO" = yes ] && pci_split "$IGPU_AUDIO_PCI"
	pci_split "$IGPU_PCI"

	# Reuse the existing domain's UUID so a re-run redefines it in place;
	# libvirt rejects an XML whose UUID does not match a domain of that name.
	local uuid
	uuid=$($VIRSH domuuid "$VM_NAME" 2>/dev/null |
		tr -d '[:space:]')
	[ -n "$uuid" ] || uuid=$(cat /proc/sys/kernel/random/uuid)

	# Pin the NIC's MAC for the same reason, and one more: Windows folds the
	# MAC into the hardware hash its licence is bound to, so letting libvirt
	# generate a fresh one on every redefine would deactivate the guest.
	local mac
	mac=$($VIRSH dumpxml --inactive "$VM_NAME" 2>/dev/null |
		sed -n "s/.*<mac address='\\([^']*\\)'.*/\\1/p" | head -1)
	[ -n "$mac" ] || mac=$(printf '52:54:00:%02x:%02x:%02x' \
		$((RANDOM % 256)) $((RANDOM % 256)) $((RANDOM % 256)))

	local xml
	xml=$(render_template domain.xml.in \
		VM_NAME "$VM_NAME" \
		UUID "$uuid" \
		MAC "$mac" \
		RAM_GB "$VM_RAM_GB" \
		VCPUS "$((ncores * 2))" \
		CORES "$ncores" \
		THREADS 2 \
		VCPUPIN "$(build_vcpupin)" \
		EMULATOR_CPUS "$VM_EMULATOR_CPUS" \
		DISK "$VM_DISK" \
		DISK_BOOT_ORDER "$([ "$installing" = yes ] && echo 2 || echo 1)" \
		INSTALL_MEDIA "$(WIN_ISO_PATH=$win_iso build_install_media "$installing")" \
		IGPU_BUS "$pci_bus" \
		IGPU_SLOT "$pci_slot" \
		IGPU_FUNC "$pci_func" \
		VBIOS "$(igpu_rom)" \
		IGPU_AUDIO_HOSTDEV "$(build_audio_hostdev)" \
		LG_SHM_NAME "$LG_SHM_NAME" \
		LG_SHM_MB "$LG_SHM_MB" \
		SHARE_DIR "$VM_SHARE_DIR") ||
		die "Could not build the domain XML."

	# libvirt's own parser is stricter than any check here, but catching a
	# malformed document first gives a better error than virsh does.
	if have xmllint; then
		xmllint --noout - <<<"$xml" ||
			die "The generated domain XML is not well-formed."
	fi

	# Keep a copy where it can be read and diffed. virsh holds the real one.
	local saved=$state_dir/$VM_NAME.xml
	printf '%s\n' "$xml" >"$saved"
	info "Domain XML written to $saved"

	# --- Define it ---------------------------------------------------------
	if $VIRSH dumpxml "$VM_NAME" >/dev/null 2>&1; then
		if vm_running; then
			warn "$VM_NAME is running; leaving its definition alone."
			warn "Shut it down and re-run phase 4 to apply changes."
			mark_done 4
			return 0
		fi
		info "Redefining the existing $VM_NAME domain."
	fi

	$VIRSH define "$saved" >/dev/null ||
		die "virsh could not define the domain. The XML is at $saved."
	info "Defined domain '$VM_NAME'."

	mark_done 4

	# A redefine stops here. Starting is the installer's business, and the
	# keypresses below would be answering a boot prompt that is not there.
	if [ "$installing" = no ]; then
		# The answer file carried the product key and the account password in
		# clear text, and the definition just written no longer points at it.
		[ -e "$UNATTEND_ISO" ] && as_root rm -f "$UNATTEND_ISO"

		say "Redefined"
		info "The new definition takes effect the next time $VM_NAME starts."
		info ""
		info "    winapp --desktop     the full Windows desktop"
		info "    $VIRSH start $VM_NAME"
		return 0
	fi

	# --- Start it and let the installation run ----------------------------
	if vm_running; then
		skip "starting $VM_NAME (already running)"
	else
		say "Starting the installation"
		$VIRSH start "$VM_NAME" >/dev/null ||
			die "Could not start $VM_NAME. Check: $VIRSH start $VM_NAME"
		info "Started. Allocating hugepages and booting the installer."
		press_any_key
	fi

	say "Installing"
	info "Watch it with:"
	info ""
	info "    virt-viewer --connect qemu:///system $VM_NAME"
	info ""
	info "It runs unattended and reboots a few times, which takes roughly"
	info "20 to 40 minutes. When Windows has settled on the desktop, run"
	info "this script again for phase 5."
}

# ---------------------------------------------------------------------------
# Phase 5: run Windows programs as Linux windows.
# ---------------------------------------------------------------------------

# Run one of the PowerShell templates inside the guest.
#
# The script is handed over base64-encoded as UTF-16LE, which is what
# -EncodedCommand expects. That sidesteps every layer of quoting between this
# shell, virsh, the agent's JSON and cmd.exe — the place where this kind of
# plumbing usually breaks.
guest_powershell() {
	local rendered=$1 timeout=${2:-900} enc

	enc=$(python3 -c '
import base64, sys
text = open(sys.argv[1], encoding="utf-8").read()
print(base64.b64encode(text.encode("utf-16-le")).decode())
' "$rendered") || return 1

	python3 "$TEMPLATE_DIR/guest-run.py" "$VM_NAME" "$timeout" \
		powershell.exe -NoProfile -NonInteractive -EncodedCommand "$enc"
}

# Run a PowerShell one-liner in the guest.
#
# Encoded for the same reason as the script above: the agent hands arguments to
# Windows as an array, which Windows then rejoins into a command line by its
# own quoting rules. A cmd.exe line with quoted Windows paths does not survive
# that trip, and comes out as nonsense like \C:\Windows\notepad.exe\*.
guest_ps_command() {
	local cmd=$1 timeout=${2:-300} enc

	enc=$(python3 -c '
import base64, sys
print(base64.b64encode(sys.argv[1].encode("utf-16-le")).decode())
' "\$ProgressPreference='SilentlyContinue'; $cmd") || return 1

	python3 "$TEMPLATE_DIR/guest-run.py" "$VM_NAME" "$timeout" \
		powershell.exe -NoProfile -NonInteractive -EncodedCommand "$enc"
}

# True once the guest agent answers, which is a long way after the VM starts.
wait_for_guest() {
	local deadline=$((SECONDS + 300))
	while [ "$SECONDS" -lt "$deadline" ]; do
		if python3 "$TEMPLATE_DIR/guest-run.py" "$VM_NAME" 20 \
			cmd.exe /c "exit 0" >/dev/null 2>&1; then
			return 0
		fi
		sleep 5
	done
	return 1
}

# Install a program's real icon where the desktop will find it.
#
# Windows executables carry their icons as PE resources at several sizes, so
# icoutils can pull out a sharp one. The alternative, asking Windows for the
# icon, returns 32x32 and looks it. The binary has to come across the shared
# folder to be read from here, which is why it is copied and then removed.
extract_icon() {
	local win_exe=$1 slug=$2
	local target="$ICON_DIR/winapp-$slug.png"

	[ -f "$target" ] && return 0
	have wrestool && have icotool && have identify && have convert || return 1

	local cache="$VM_SHARE_DIR/.winapp-cache"
	local staged="$cache/$slug.exe"
	mkdir -p "$cache"

	# The guest copies its own binary into the folder both sides can see, so
	# it can be read from here without any of it going over the network.
	guest_ps_command "Copy-Item -LiteralPath '$win_exe' \
		-Destination 'Z:\\.winapp-cache\\$slug.exe' -Force" >/dev/null 2>&1
	[ -f "$staged" ] || {
		rmdir "$cache" 2>/dev/null
		return 1
	}

	local work
	work=$(mktemp -d)
	# Resource type 14 is the icon group: the set of sizes one icon ships in.
	if wrestool -x -t 14 -o "$work" "$staged" >/dev/null 2>&1; then
		local ico
		for ico in "$work"/*.ico; do
			[ -f "$ico" ] || continue
			icotool -x -o "$work" "$ico" >/dev/null 2>&1 || true
		done

		# Pick by actual pixel width rather than file size: a large but
		# low-colour 48x48 can outweigh the 256x256 that is wanted.
		local best
		best=$(find "$work" -name '*.png' -print0 2>/dev/null |
			xargs -0 -r identify -format '%w %i\n' 2>/dev/null |
			sort -rn | head -1 | cut -d' ' -f2-)

		if [ -n "$best" ]; then
			mkdir -p "$ICON_DIR"
			convert "$best" -resize 256x256\> -background none \
				-gravity center -extent 256x256 "$target" 2>/dev/null ||
				cp "$best" "$target"
		fi
	fi

	rm -rf "$work" "$staged"
	rmdir "$cache" 2>/dev/null
	[ -f "$target" ]
}

# Short identifier for a Windows program, derived from its executable name.
#
# The same rule has to be used for the icon file, the desktop entry and the WM
# class the launcher hands to FreeRDP: the desktop knows a window belongs to a
# launcher entry only because StartupWMClass matches, and a mismatch shows the
# running program as a second, nameless entry in the taskbar.
#
# Note the backslashes: basename splits on forward slashes, so on a path like
# C:\Windows\System32\notepad.exe it returns the whole string and the slug ends
# up as "cwindowssystem32notepad".
winapp_slug() {
	local name=${1##*\\}
	name=${name%.*}
	printf '%s' "$name" | tr '[:upper:]' '[:lower:]' | tr -cd 'a-z0-9-'
}

# Categories and file types for the programs worth wiring into the desktop, so
# that a spreadsheet in the file manager opens in Excel rather than nothing.
winapp_categories() {
	case "$1" in
	Excel | Word | PowerPoint | Outlook | OneNote | Access) echo "Office;" ;;
	"PDF-XChange Editor") echo "Office;Viewer;" ;;
	"File Explorer") echo "System;FileTools;" ;;
	*) echo "Utility;" ;;
	esac
}

winapp_mimetypes() {
	case "$1" in
	Excel) echo "application/vnd.ms-excel;application/vnd.openxmlformats-officedocument.spreadsheetml.sheet;text/csv;" ;;
	Word) echo "application/msword;application/vnd.openxmlformats-officedocument.wordprocessingml.document;application/rtf;" ;;
	PowerPoint) echo "application/vnd.ms-powerpoint;application/vnd.openxmlformats-officedocument.presentationml.presentation;" ;;
	"PDF-XChange Editor") echo "application/pdf;" ;;
	*) echo "" ;;
	esac
}

phase_5() {
	say "Phase 5: run Windows programs as Linux windows"

	phase_done 4 || die "Phase 4 has not run: there is no VM yet."

	# --- The guest has to be up to be configured -------------------------
	if ! vm_running; then
		info "Starting $VM_NAME."
		$VIRSH start "$VM_NAME" >/dev/null || die "Could not start $VM_NAME."
	fi

	info "Waiting for the guest agent."
	wait_for_guest ||
		die "The guest is not answering. Check it with:
       virt-viewer --connect qemu:///system $VM_NAME"
	info "Guest is up."

	# Windows answered, so there is an operating system on that disk. Record
	# it: this is what stops phase 4 from ever attaching the installer again.
	if [ ! -f "$installed_marker" ]; then
		mkdir -p "$state_dir"
		date -Is >"$installed_marker"
		info "Recorded that Windows is installed; the installer will not be"
		info "attached again. Delete $installed_marker to reinstall."
		warn "The domain still has the installation media attached, and the"
		warn "answer file it boots wipes disk 0 without asking. Shut Windows"
		warn "down and run '$0 --only 4' to detach it."
	fi

	# The answer file holds the product key and the account password in clear
	# text. Once phase 4 has redefined the domain without it, it has no reason
	# to exist; phase 3 rebuilds it if Windows is ever reinstalled.
	if [ -e "$UNATTEND_ISO" ] &&
		! $VIRSH dumpxml --inactive "$VM_NAME" 2>/dev/null |
		grep -qF "$UNATTEND_ISO"; then
		as_root rm -f "$UNATTEND_ISO"
		info "Removed $UNATTEND_ISO; it held the key and password in clear text."
	fi

	# --- Configure the guest ---------------------------------------------
	local rendered
	rendered=$(mktemp --suffix=.ps1)
	render_template guest-setup.ps1 WINFSP_URL "$WINFSP_URL" >"$rendered" ||
		{
			rm -f "$rendered"
			die "Could not build the guest setup script."
		}

	info "Configuring the guest (shared folder, RemoteApp, sessions)."
	# Windows ends its lines with CRLF, and a stray carriage return stops the
	# markers below from matching, which quietly yields an empty program list.
	# pipefail is on, so the guest's exit status still decides this.
	local report
	report=$(guest_powershell "$rendered" 900 | tr -d '\r') || {
		rm -f "$rendered"
		die "Guest setup failed:
$report"
	}
	rm -f "$rendered"
	sed -n '/^  /p' <<<"$report"

	# --- The launcher ------------------------------------------------------
	mkdir -p "$BIN_DIR"
	if render_template winapp \
		VM_NAME "$VM_NAME" \
		WIN_USER "$WIN_USER" \
		SHARE_DIR "$VM_SHARE_DIR" \
		PW_FILE "$pw_file" >"$BIN_DIR/winapp.tmp"; then
		chmod 755 "$BIN_DIR/winapp.tmp"
		mv -f "$BIN_DIR/winapp.tmp" "$BIN_DIR/winapp"
		info "Installed $BIN_DIR/winapp"
	else
		rm -f "$BIN_DIR/winapp.tmp"
		die "Could not build the launcher."
	fi

	case ":$PATH:" in
	*":$BIN_DIR:"*) ;;
	*) warn "$BIN_DIR is not on your PATH; 'winapp' will not be found." ;;
	esac

	# --- Desktop entries ---------------------------------------------------
	mkdir -p "$APP_DIR"

	local made=0 name win_exe slug icon exec_path
	while IFS=$'\t' read -r name win_exe; do
		[ -n "$name" ] && [ -n "$win_exe" ] || continue

		# Must match the WM class the launcher passes to FreeRDP, or the
		# window will not be matched to its launcher entry and the taskbar
		# shows it as a second, nameless application.
		slug=$(winapp_slug "$win_exe")

		# The fallback, for a program whose icon could not be pulled out of
		# its .exe. Nothing here ever creates a "winapp-windows" icon, which
		# is what this named until now: a dangling name, which a theme answers
		# with its broken-image placeholder. This one is in the icon naming
		# spec, so every theme has it.
		icon=application-x-executable
		extract_icon "$win_exe" "$slug" && icon="winapp-$slug"

		# Exec goes through two unescapings, so a backslash in it is spelled
		# with four. The key file reader turns \\\\ into \\, and the shell-style
		# split of the Exec value then turns that into the single backslash a
		# Windows path wants. Written literally, as one, the first of those two
		# reads it as the start of an escape sequence, finds \W is not one, and
		# rejects the key outright: the entry ends up with no Exec at all and
		# clicking it does nothing. That is how all of these looked to GNOME, to
		# gio, and to any file manager opening a spreadsheet through the
		# MimeType below. KDE is more forgiving and ran them anyway, which is
		# why it went unnoticed.
		#
		# X-WinApp-Program is read by 'winapp --list' with sed, never by a
		# desktop-file parser, so it stays as it is.
		exec_path=${win_exe//\\/\\\\\\\\}

		cat >"$APP_DIR/winapp-$slug.desktop" <<-EOF
			[Desktop Entry]
			Type=Application
			Name=$name
			GenericName=$name (Windows)
			Comment=$name, running in the $VM_NAME virtual machine
			Exec=$BIN_DIR/winapp "$exec_path" %F
			Icon=$icon
			Terminal=false
			NoDisplay=false
			Categories=$(winapp_categories "$name")
			MimeType=$(winapp_mimetypes "$name")
			StartupNotify=true
			StartupWMClass=winapp-$slug
			X-WinApp=true
			X-WinApp-Program=$win_exe
		EOF
		made=$((made + 1))
	done < <(sed -n '/^---PROGRAMS---$/,/^---END---$/p' <<<"$report" |
		sed '1d;$d')

	info "Created $made launcher entries in $APP_DIR"

	# The whole desktop, for the times when a program is not enough: driver
	# installs, Windows Update, anything that wants a real machine.
	cat >"$APP_DIR/winapp-desktop.desktop" <<-EOF
		[Desktop Entry]
		Type=Application
		Name=Windows 11 Desktop
		Comment=The full Windows desktop from $VM_NAME, in a window
		Exec=$BIN_DIR/winapp --desktop
		Icon=computer
		Terminal=false
		Categories=System;
		StartupNotify=true
		X-WinApp=true
	EOF

	have update-desktop-database &&
		update-desktop-database "$APP_DIR" >/dev/null 2>&1
	# The cache is rebuilt for the theme, two levels above the size directory.
	have gtk-update-icon-cache &&
		gtk-update-icon-cache -f -t "${ICON_DIR%/*/*}" >/dev/null 2>&1

	mark_done 5

	say "Windows programs are on your desktop"
	info "Anything found in the guest now has a launcher entry. Also:"
	info ""
	info "    winapp --list                 what is wired up"
	info "    winapp --desktop              the whole Windows desktop"
	info "    winapp 'C:\\path\\to\\app.exe'   anything else"
	info ""
	info "Files in $VM_SHARE_DIR are visible to Windows as drive Z:, and a"
	info "file passed to winapp from anywhere else is shared just for that run."
	info ""
	info "Install Office or PDF-XChange in the guest, then re-run this phase"
	info "to pick them up:  $0 --only 5"
}

# ---------------------------------------------------------------------------
# Phase 6: Looking Glass.
# ---------------------------------------------------------------------------

# The size of the monitor this is running on, as "WIDTHxHEIGHT". The virtual
# monitor in the guest is made to match, so the client draws guest pixels onto
# host pixels one for one rather than rescaling every frame.
host_resolution() {
	local geom=""
	if have kscreen-doctor; then
		# Prints "Geometry: 0,0 2560x1440"; the size is the last field.
		geom=$(kscreen-doctor -o 2>/dev/null |
			awk '/Geometry:/ {print $NF; exit}')
	fi
	if [ -z "$geom" ] && have wlr-randr; then
		geom=$(wlr-randr 2>/dev/null |
			awk '/current/ {print $1; exit}')
	fi
	if [ -z "$geom" ] && have xrandr; then
		geom=$(xrandr 2>/dev/null | awk '/\*/ {print $1; exit}')
	fi
	# Take the first WIDTHxHEIGHT and ignore whatever the tool put around it:
	# a refresh rate, a "px," suffix, an interlace flag. Deleting the other
	# characters instead, which is what this did, splices 2560x1440@60 into
	# 2560x144060 and hands that to the virtual monitor as a height.
	if [[ $geom =~ ([0-9]+)x([0-9]+) ]]; then
		printf '%sx%s' "${BASH_REMATCH[1]}" "${BASH_REMATCH[2]}"
	else
		printf '1920x1080'
	fi
}

# Compile the Looking Glass client. Nothing packages it, and the client and the
# host application have to come from the same release or they refuse to talk.
build_lg_client() {
	local bin=$BIN_DIR/looking-glass-client version

	if [ -x "$bin" ]; then
		version=$("$bin" --help 2>&1 |
			sed -n 's/.*Looking Glass (\([^)]*\)).*/\1/p' | head -n1)
		if [ -n "$version" ]; then
			skip "Looking Glass client $version"
			return 0
		fi
	fi

	info "Building the Looking Glass client; this takes a few minutes."
	local work
	work=$(mktemp -d) || die "Could not create a build directory."

	curl -fL --proto '=https' --proto-redir '=https' \
		--progress-bar --retry 3 --retry-delay 2 \
		-o "$work/source.tar.gz" "$LG_SOURCE_URL" ||
		{
			rm -rf "$work"
			die "Could not download $LG_SOURCE_URL"
		}
	tar -xzf "$work/source.tar.gz" -C "$work" ||
		{
			rm -rf "$work"
			die "The Looking Glass source archive did not unpack."
		}

	local src
	src=$(find "$work" -mindepth 1 -maxdepth 1 -type d | head -n1)
	[ -n "$src" ] && [ -d "$src/client" ] ||
		{
			rm -rf "$work"
			die "No client directory in the Looking Glass source archive."
		}

	# B7 vendors a copy of nanosvg.h that GCC 13 and later warn about, and the
	# build turns warnings into errors. The warning is in that vendored header,
	# not in Looking Glass, so it is downgraded rather than chased.
	(
		mkdir -p "$src/client/build" &&
			cd "$src/client/build" &&
			cmake -DCMAKE_BUILD_TYPE=Release \
				-DCMAKE_C_FLAGS="-Wno-error=maybe-uninitialized" .. &&
			make -j"$(nproc)"
	) >"$work/build.log" 2>&1 ||
		{
			tail -n 30 "$work/build.log" >&2
			rm -rf "$work"
			die "The Looking Glass client did not build; last lines above."
		}

	mkdir -p "$BIN_DIR"
	install -m 755 "$src/client/build/looking-glass-client" "$bin" ||
		{
			rm -rf "$work"
			die "Could not install $bin"
		}
	rm -rf "$work"
	info "Installed $bin"
}

# Where installers are staged on their way into the guest. A subdirectory
# rather than the share itself: that directory is the user's, and setup files
# accumulating in it would be clutter they did not put there.
setup_share() {
	printf '%s/.setup' "$VM_SHARE_DIR"
}

# Copy a file into the staging area and give back the path Windows sees it at.
# Everything handed to the guest goes this way: the guest agent can run
# programs but cannot carry a file across.
share_to_guest() {
	local src=$1 name=${2:-$(basename "$1")} dir
	dir=$(setup_share)
	mkdir -p "$dir" || return 1
	install -m 644 "$src" "$dir/$name" || return 1
	printf 'Z:\\.setup\\%s' "$name"
}

# Fetch a URL into the staging area once, and give back its Windows path.
fetch_to_guest() {
	local url=$1 name=$2 dir
	dir=$(setup_share)
	mkdir -p "$dir" || return 1
	if [ ! -s "$dir/$name" ]; then
		curl -fL --proto '=https' --proto-redir '=https' \
			--progress-bar --retry 3 --retry-delay 2 \
			-o "$dir/$name.part" "$url" ||
			{
				rm -f "$dir/$name.part"
				return 1
			}
		mv -f "$dir/$name.part" "$dir/$name"
	fi
	chmod 644 "$dir/$name"
	printf 'Z:\\.setup\\%s' "$name"
}

# True when the guest already has a virtual display device.
guest_has_vdd() {
	guest_ps_command 'if (Get-PnpDevice -Class Display -ErrorAction SilentlyContinue |
		Where-Object { $_.InstanceId -like "ROOT\DISPLAY\*" }) { "yes" }' 2>/dev/null |
		tr -d '\r' | grep -q yes
}

phase_6() {
	say "Phase 6: Looking Glass"

	phase_done 4 || die "Phase 4 has not run: there is no VM yet."

	# --- The client, on this side ----------------------------------------
	build_lg_client

	# --- The guest has to be up to be configured -------------------------
	if ! vm_running; then
		info "Starting $VM_NAME."
		$VIRSH start "$VM_NAME" >/dev/null || die "Could not start $VM_NAME."
	fi
	info "Waiting for the guest agent."
	wait_for_guest ||
		die "The guest is not answering. Check it with:
       virt-viewer --connect qemu:///system $VM_NAME"

	mkdir -p "$VM_SHARE_DIR"

	local guest_dir='C:\VirtualDisplayDriver'

	# --- The virtual display ----------------------------------------------
	# Without this the guest has no display on the passed-through GPU at all:
	# there is no cable on it, so Windows gives it no outputs and draws
	# nothing, and Looking Glass has nothing to capture.
	if guest_has_vdd; then
		skip "virtual display driver"
	else
		info "Installing the virtual display driver in the guest."
		local vdd_zip
		vdd_zip=$(fetch_to_guest "$VDD_URL" "vdd-driver.zip") ||
			die "Could not download $VDD_URL"

		local vdd_installer
		vdd_installer=$(share_to_guest "$TEMPLATE_DIR/install-vdd.ps1") ||
			die "Could not stage install-vdd.ps1"

		# The archive has the files one directory down; the driver insists on
		# finding them directly under C:\VirtualDisplayDriver.
		guest_ps_command "
			New-Item -ItemType Directory -Force -Path '$guest_dir' | Out-Null
			Expand-Archive -Path '$vdd_zip' -DestinationPath '$guest_dir' -Force
			Get-ChildItem -Directory '$guest_dir' | ForEach-Object {
				Move-Item -Path (Join-Path \$_.FullName '*') -Destination '$guest_dir' -Force
				Remove-Item \$_.FullName -Recurse -Force
			}
			Copy-Item '$vdd_installer' '$guest_dir\\install-vdd.ps1' -Force" 300 >/dev/null ||
			die "Could not unpack the virtual display driver in the guest."

		local report
		report=$(guest_ps_command "& powershell.exe -NoProfile -NonInteractive \
			-ExecutionPolicy Bypass -File '$guest_dir\\install-vdd.ps1' \
			-InfPath '$guest_dir\\MttVDD.inf' 2>&1 | Out-String" 600 | tr -d '\r')
		guest_has_vdd ||
			die "The virtual display driver did not install:
$report"
		info "Virtual display driver installed."
	fi

	# --- What the virtual display looks like ------------------------------
	# Rewritten every run: the monitor this is run on may have changed, and
	# the driver only reads the file when it starts.
	local geom width height
	geom=$(host_resolution)
	width=${LG_WIDTH:-${geom%x*}}
	height=${LG_HEIGHT:-${geom#*x}}
	info "Virtual monitor: ${width}x${height}, rendered on the passed-through GPU."

	local settings
	settings=$(mktemp --suffix=.xml)
	if render_template vdd_settings.xml.in \
		GPU_NAME "$IGPU_GUEST_NAME" \
		WIDTH "$width" HEIGHT "$height" >"$settings"; then
		local guest_settings
		guest_settings=$(share_to_guest "$settings" "vdd_settings.xml") ||
			die "Could not stage vdd_settings.xml"
		guest_ps_command "
			Copy-Item '$guest_settings' '$guest_dir\\vdd_settings.xml' -Force
			\$d = Get-PnpDevice -Class Display | Where-Object { \$_.InstanceId -like 'ROOT\\DISPLAY\\*' }
			Disable-PnpDevice -InstanceId \$d.InstanceId -Confirm:\$false
			Start-Sleep -Seconds 3
			Enable-PnpDevice -InstanceId \$d.InstanceId -Confirm:\$false
			Start-Sleep -Seconds 5" 300 >/dev/null ||
			warn "Could not apply the virtual display settings."
		rm -f "$settings"
	else
		rm -f "$settings"
		die "Could not build vdd_settings.xml."
	fi

	# --- The host application, in the guest -------------------------------
	if guest_ps_command 'if (Get-Service -Name "Looking Glass (host)" -ErrorAction SilentlyContinue) { "yes" }' |
		tr -d '\r' | grep -q yes; then
		skip "Looking Glass host application"
	else
		info "Installing the Looking Glass host application in the guest."
		local work
		work=$(mktemp -d)
		curl -fL --proto '=https' --proto-redir '=https' \
			--progress-bar --retry 3 --retry-delay 2 \
			-o "$work/host.zip" "$LG_HOST_URL" ||
			{
				rm -rf "$work"
				die "Could not download $LG_HOST_URL"
			}
		# Since B6 this installer carries the IVSHMEM driver too, which is
		# what lets the guest see the shared memory at all.
		# 7z rather than unzip: it is already a dependency, for reading the
		# installation ISOs, and unzip is not.
		7z e -y -o"$work" "$work/host.zip" looking-glass-host-setup.exe \
			>/dev/null ||
			{
				rm -rf "$work"
				die "No host installer inside the Looking Glass host archive."
			}
		local host_exe
		host_exe=$(share_to_guest "$work/looking-glass-host-setup.exe") ||
			{
				rm -rf "$work"
				die "Could not stage the Looking Glass host installer."
			}
		rm -rf "$work"

		guest_ps_command "
			\$p = Start-Process -FilePath '$host_exe' -ArgumentList '/S' -Wait -PassThru -NoNewWindow
			'exit ' + \$p.ExitCode" 600 >/dev/null ||
			die "The Looking Glass host installer failed."
		guest_ps_command 'if (Get-Service -Name "Looking Glass (host)" -ErrorAction SilentlyContinue) { "yes" }' |
			tr -d '\r' | grep -q yes ||
			die "The Looking Glass host service is not there after installing."
		info "Looking Glass host application installed."
	fi

	# --- One display, not two ---------------------------------------------
	# Mouse input arrives over SPICE, whose coordinates span the whole guest
	# desktop, while the client only shows the virtual monitor. With the
	# emulated adapter also attached, the pointer lands on a screen that is
	# not on show: hovering works and every click goes somewhere else.
	guest_ps_command '
		$qxl = Get-PnpDevice -Class Display -ErrorAction SilentlyContinue |
			Where-Object { $_.InstanceId -like "PCI\VEN_1B36&DEV_0100*" -and $_.Status -eq "OK" }
		if ($qxl) {
			Disable-PnpDevice -InstanceId $qxl.InstanceId -Confirm:$false
			"disabled"
		} else { "already off" }' 300 >/dev/null ||
		warn "Could not disable the emulated display adapter."

	# --- Make the virtual monitor the one Windows draws on ----------------
	local runner setter
	runner=$(share_to_guest "$TEMPLATE_DIR/run-in-session.ps1") &&
		setter=$(share_to_guest "$TEMPLATE_DIR/set-display.ps1") ||
		die "Could not stage the display scripts."
	guest_ps_command "
		Copy-Item '$runner' '$guest_dir\\run-in-session.ps1' -Force
		Copy-Item '$setter' '$guest_dir\\set-display.ps1' -Force" 120 >/dev/null ||
		warn "Could not copy the display scripts into the guest."

	# Only works with somebody signed in: display configuration lives in the
	# interactive session, and the guest agent runs in session 0 where there
	# is no desktop to configure.
	local display_report
	display_report=$(guest_ps_command "& powershell.exe -NoProfile -NonInteractive \
		-ExecutionPolicy Bypass -File '$guest_dir\\run-in-session.ps1' \
		-ScriptPath '$guest_dir\\set-display.ps1' \
		-ScriptArgs '-Width $width -Height $height' 2>&1 | Out-String" 300 | tr -d '\r')
	if grep -q 'last result   : 0x0' <<<"$display_report"; then
		info "Virtual monitor set to ${width}x${height} and made primary."
	else
		warn "Could not set the virtual monitor's mode. Sign in to Windows and"
		warn "re-run this phase:  $0 --only 6"
	fi

	# --- The launcher ------------------------------------------------------
	mkdir -p "$BIN_DIR"
	if render_template looking-glass \
		VM_NAME "$VM_NAME" \
		LG_SHM "/dev/shm/$LG_SHM_NAME" \
		LG_CLIENT "$BIN_DIR/looking-glass-client" >"$BIN_DIR/looking-glass.tmp"; then
		chmod 755 "$BIN_DIR/looking-glass.tmp"
		mv -f "$BIN_DIR/looking-glass.tmp" "$BIN_DIR/looking-glass"
		info "Installed $BIN_DIR/looking-glass"
	else
		rm -f "$BIN_DIR/looking-glass.tmp"
		die "Could not build the Looking Glass launcher."
	fi

	mkdir -p "$APP_DIR"
	cat >"$APP_DIR/looking-glass.desktop" <<-EOF
		[Desktop Entry]
		Type=Application
		Name=Windows (Looking Glass)
		Comment=The $VM_NAME desktop, drawn by the passed-through GPU
		Exec=$BIN_DIR/looking-glass
		Icon=computer
		Terminal=false
		Categories=System;
		StartupNotify=true
	EOF
	have update-desktop-database &&
		update-desktop-database "$APP_DIR" >/dev/null 2>&1

	mark_done 6

	say "Looking Glass is ready"
	info "Run it with:"
	info ""
	info "    looking-glass"
	info ""
	info "Frames are rendered by the passed-through iGPU and handed over shared"
	info "memory, so nothing is ever encoded. Keyboard and mouse go over SPICE."
	info ""
	info "The emulated display was switched off, so this is now the only screen"
	info "Windows has. If it ever fails to come up, the guest agent still works"
	info "no matter what is on screen, so the old console can be brought back:"
	info ""
	info "    $0 --enable-console"
	info ""
	warn "The iGPU cannot be reset. Once Windows has driven it, restarting the"
	warn "VM leaves the card wedged and the guest reports error 43; only a host"
	warn "reboot clears it. Leave the VM running rather than cycling it."
}

# Switch the emulated display adapter back on.
#
# Phase 6 turns it off so that the guest has a single screen and SPICE's
# pointer coordinates line up with what Looking Glass shows. That leaves no
# fallback if the virtual display ever fails to start, so this puts it back.
# The guest agent is not a display client and keeps working either way.
enable_console() {
	vm_running || die "$VM_NAME is not running."
	wait_for_guest || die "The guest agent is not answering."
	guest_ps_command '
		$qxl = Get-PnpDevice -Class Display -ErrorAction SilentlyContinue |
			Where-Object { $_.InstanceId -like "PCI\VEN_1B36&DEV_0100*" }
		if (-not $qxl) { throw "no emulated display adapter found" }
		Enable-PnpDevice -InstanceId $qxl.InstanceId -Confirm:$false
		"enabled"' 300 >/dev/null ||
		die "Could not enable the emulated display adapter."
	say "The emulated display is back"
	info "Look at it with:  virt-viewer --connect qemu:///system $VM_NAME"
	info ""
	info "Note that with two screens attached, the Looking Glass pointer will"
	info "not line up again until this is switched back off:  $0 --only 6"
}

# ---------------------------------------------------------------------------
# Driver.
# ---------------------------------------------------------------------------

last_phase=6
usage() {
	# The header comment at the top of this file, down to the author line, is
	# the help text. Matching on that line rather than counting lines means the
	# help does not go stale the moment the header grows a paragraph.
	sed -n '3,/^# Author:/p' "${BASH_SOURCE[0]}" | sed -e '$d' -e 's/^# \?//'
	exit "${1:-0}"
}

# Print every setting and the value it has for this run. The list is read out
# of this file's own configuration block, so a setting added there appears here
# without having to be named a second time.
show_config() {
	local name
	echo "Settings for this run. Override any of them from the environment:"
	echo
	echo "    VM_RAM_GB=16 IGPU_PCI=0000:0a:00.0 $0"
	echo
	while read -r name; do
		printf '  %-22s %s\n' "$name" "${!name}"
	done < <(sed -n 's/^: "${\([A-Z_]*\):=.*/\1/p' "${BASH_SOURCE[0]}")
}

show_status() {
	local p
	echo "Progress recorded in $state_dir"
	for p in $(seq 0 $last_phase); do
		if phase_done "$p"; then
			printf '  phase %s  done   (%s)\n' "$p" "$(cat "$state_dir/phase-$p.done")"
		else
			printf '  phase %s  pending\n' "$p"
		fi
	done
}

# A phase number this script actually has. Without this, a typo runs off the
# end as "phase_x: command not found" or hands seq something it cannot count.
valid_phase() { [[ "$1" =~ ^[0-9]+$ ]] && [ "$1" -le "$last_phase" ]; }

main() {
	# forced tells an explicit --from from the default. Without it "--from 0"
	# quietly skipped the phases it promised to re-run, because the loop below
	# used from=0 itself as the signal for "this is an ordinary run".
	local from=0 only="" forced=0
	while [ $# -gt 0 ]; do
		case $1 in
		--status)
			show_status
			exit 0
			;;
		--config)
			show_config
			exit 0
			;;
		--reset)
			rm -f "$state_dir"/phase-*.done
			echo "Progress cleared."
			exit 0
			;;
		--enable-console)
			enable_console
			exit 0
			;;
		--only)
			only=${2:-}
			valid_phase "$only" ||
				die "--only takes a phase number from 0 to $last_phase."
			shift 2
			;;
		--from)
			from=${2:-}
			valid_phase "$from" ||
				die "--from takes a phase number from 0 to $last_phase."
			forced=1
			shift 2
			;;
		-h | --help) usage ;;
		*) die "Unknown option: $1 (try --help)" ;;
		esac
	done

	local p
	if [ -n "$only" ]; then
		"phase_$only"
		exit
	fi
	for p in $(seq "$from" $last_phase); do
		if [ "$forced" -eq 0 ] && phase_done "$p"; then
			skip "Phase $p"
			continue
		fi
		"phase_$p"
	done
}

main "$@"
