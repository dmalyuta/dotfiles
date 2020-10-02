#!/bin/bash
# Reload the WiFi kernel driver. Useful if WiFi drops out after suspend, for
# example. Based on the answer here:
#  https://askubuntu.com/a/893823/596413
#
# Two helpful things to prevent needing to use this script:
#  - Set REGDOMAIN=US: https://superuser.com/questions/974017/ubuntu-wlan0-authenticates-and-then-drops/974033#974033
#  - Disable IPv6 via grub: https://linuxconfig.org/how-to-disable-ipv6-address-on-ubuntu-20-04-lts-focal-fossa

WIFI_KERNEL_DRIVER=ath10k_pci

if (lspci -knn | grep Net -A2 | grep $WIFI_KERNEL_DRIVER); then
    sudo rmmod $WIFI_KERNEL_DRIVER && \
	sudo modprobe $WIFI_KERNEL_DRIVER
fi
