#!/bin/bash
# Reload the WiFi kernel driver. Useful if WiFi drops out after suspend, for
# example. Based on the answer here:
#  https://askubuntu.com/a/893823/596413
#
# Two helpful things to prevent needing to use this script:
#  - Set REGDOMAIN=US: https://superuser.com/questions/974017/ubuntu-wlan0-authenticates-and-then-drops/974033#974033
#  - Disable IPv6 via grub: https://linuxconfig.org/how-to-disable-ipv6-address-on-ubuntu-20-04-lts-focal-fossa
#
# Both of the above seem to not help... (still dropping WiFi). Maybe promising to update the firmware:
#
#   $ sudo su
#   $ cd /lib/firmware/ath10k/QCA6174/hw3.0/
#   $ git clone https://github.com/kvalo/ath10k-firmware.git
#   $ mv ./board-2.bin ./board-2.bin.bak
#   $ cp ./ath10k-firmware/QCA6174/hw3.0/board-2.bin ./board-2.bin
#   $ mv ./firmware-6.bin ./firmware-6.bin.bak
#   $ cp ./ath10k-firmware/QCA6174/hw3.0/4.4.1/firmware-6.bin_WLAN.RM.4.4.1-00128-QCARMSWPZ-1 ./firmware-6.bin
#   $ rm -rf ./ath10k-firmware
#   $ exit
#
# (you can check which firmware you're using via: # sudo ethtool -i <wifi_name>) where
# ethtool can be installed from apt and <wifi_name> is obtained from ifconfig, e.g.
# wlp58s0.

WIFI_KERNEL_DRIVER=ath10k_pci

if (lspci -knn | grep Net -A2 | grep $WIFI_KERNEL_DRIVER); then
    sudo rmmod $WIFI_KERNEL_DRIVER && \
	sudo modprobe $WIFI_KERNEL_DRIVER
fi
