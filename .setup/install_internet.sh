#!/bin/bash
# ----------------------------------------------------------------------
#
# Installation of Internet/Web applications.
#
# ----------------------------------------------------------------------

echo_prefix_temp="$echo_prefix"
echo_prefix="[internet setup] "

# Google Chrome
if program_not_installed "google-chrome"; then
    apt_get_install_pkg libxss1
    apt_get_install_pkg libappindicator1
    apt_get_install_pkg libindicator7
    runcmd "wget https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb -O /tmp/chrome.deb"
    runcmd_noexit "dpkg -i /tmp/chrome.deb" nonull
    runcmd "apt-get --assume-yes install -f" nonull
fi

# Dropbox
if program_not_installed "dropbox"; then
	runcmd "wget https://www.dropbox.com/download?dl=packages/ubuntu/dropbox_2015.10.28_amd64.deb -O /tmp/dropbox.deb"
    runcmd_noexit "dpkg -i /tmp/dropbox.deb" nonull
    runcmd "apt-get --assume-yes install -f" nonull
fi

echo_prefix="$echo_prefix_temp"
