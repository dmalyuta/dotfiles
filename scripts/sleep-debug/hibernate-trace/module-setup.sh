#!/bin/bash
# dracut module: save the pm_trace of the last hibernation before resuming it, see
# hibernate-trace-save. Included through add_dracutmodules, by ../setup.sh.

check() { return 255; }

depends() { echo systemd; }

installkernel() {
	# The chipset's watchdog, which it asks whether it fired.
	instmods sp5100_tco
}

install() {
	inst_multiple dmesg grep head cat ls tr chattr rm dd systemctl modprobe
	inst_script "$moddir/hibernate-trace-save" /usr/bin/hibernate-trace-save
	inst_simple "$moddir/hibernate-trace-save.service" \
		"$systemdsystemunitdir/hibernate-trace-save.service"
	$SYSTEMCTL -q --root "$initdir" add-wants sysinit.target hibernate-trace-save.service
}
