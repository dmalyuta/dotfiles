#!/bin/bash
# dracut module: keep the ASUS keyboard backlight dark from the initramfs on, see
# asus-kbd-dark-initrd.c. Included through add_dracutmodules, by scripts/configure_hardware, which
# also builds the program.

program=/usr/local/lib/asus-kbd-dark/asus-kbd-dark-initrd

check() { return 255; }

depends() { echo systemd; }

installkernel() {
	# The keyboard controller's driver, which the program waits for.
	instmods hid_asus
}

install() {
	inst_simple "$program" /usr/bin/asus-kbd-dark-initrd
	inst_simple "$moddir/asus-kbd-dark-initrd.service" \
		"$systemdsystemunitdir/asus-kbd-dark-initrd.service"
	$SYSTEMCTL -q --root "$initdir" add-wants sysinit.target asus-kbd-dark-initrd.service
}
