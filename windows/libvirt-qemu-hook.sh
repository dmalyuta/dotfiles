#!/bin/bash
#
# Allocate hugepages for the guest on the way up and release them on the way
# down. Written by make_windows_vm.sh; edit that instead of this.
#
# libvirt calls this as: $1=domain $2=operation $3=sub-operation $4=extra,
# with the domain XML on stdin. For "prepare begin" anything printed on stdout
# replaces the XML libvirt goes on to use.

domain=$1
operation=$2
vm={{VM_NAME}}
ram_gb={{VM_RAM_GB}}
log=/var/log/libvirt/hugepage-hook.log

# Always drain stdin, whichever hook point this is. libvirt writes the domain
# XML to every one of them, and a hook that exits without reading leaves
# libvirt writing into a closed pipe.
xml=$(cat)

[ "$domain" = "$vm" ] || exit 0

note() { echo "$(date -Is) $*" >>"$log"; }

pool() { echo /sys/kernel/mm/hugepages/hugepages-${1}kB/nr_hugepages; }

# Ask the kernel for n pages of the given size. It may hand back fewer than
# asked for without failing, so compare what landed against what was wanted.
reserve() {
	local kb=$1 want=$2 got
	[ -w "$(pool "$kb")" ] || return 1
	echo "$want" >"$(pool "$kb")" 2>/dev/null
	got=$(cat "$(pool "$kb")")
	[ "$got" -ge "$want" ]
}

release() {
	local kb=$1
	[ -w "$(pool "$kb")" ] && echo 0 >"$(pool "$kb")" 2>/dev/null
	return 0
}

case "$operation" in
prepare)
	# Compacting first is what makes gigabyte pages achievable on a machine
	# that has been up a while; without it the allocation usually comes up
	# short against a fragmented free list.
	echo 1 >/proc/sys/vm/compact_memory 2>/dev/null
	sleep 1

	if reserve 1048576 "$ram_gb"; then
		note "$vm: reserved $ram_gb x 1G hugepages"
		# Nothing to change; libvirt keeps the XML it already has.
		exit 0
	fi

	release 1048576
	if reserve 2048 $((ram_gb * 512)); then
		note "$vm: 1G pages unavailable, fell back to $((ram_gb * 512)) x 2M"
		# Point the domain at the 2 MiB pool instead of the 1 GiB one.
		echo "$xml" |
			sed "s|<page size='1048576' unit='KiB'/>|<page size='2048' unit='KiB'/>|"
		exit 0
	fi

	release 2048
	note "$vm: no hugepages available, starting without them"
	# Drop the <hugepages> element but keep the <access mode='shared'/> that
	# virtiofs needs in order to map the guest's memory.
	echo "$xml" | sed "/<hugepages>/,/<\/hugepages>/d"
	;;
release)
	release 1048576
	release 2048
	note "$vm: released hugepages"
	;;
esac
exit 0
