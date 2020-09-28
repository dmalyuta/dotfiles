#!/bin/bash
# Reload the WiFi kernel driver. Useful if WiFi drops out after suspend, for
# example. Based on the answer here:
#  https://askubuntu.com/a/893823/596413

WIFI_KERNEL_DRIVER=ath10k_pci

if (lspci -knn | grep Net -A2 | grep $WIFI_KERNEL_DRIVER); then
    sudo rmmod $WIFI_KERNEL_DRIVER && \
	sudo modprobe $WIFI_KERNEL_DRIVER
fi
