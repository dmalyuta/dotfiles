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
    runcmd "wget https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb -O ${home}/Downloads/chrome.deb"
    runcmd_noexit "dpkg -i ${home}/Downloads/chrome.deb" nonull
    runcmd "apt-get --assume-yes install -f" nonull
    runcmd "rm -f ${home}/Downloads/chrome.deb"
fi

# Insync Google Drive client
if program_not_installed "insync"; then
	runcmd "wget https://d2t3ff60b2tol4.cloudfront.net/builds/insync_1.3.16.36155-trusty_amd64.deb -O ${home}/Downloads/insync.deb"
    runcmd_noexit "dpkg -i ${home}/Downloads/insync.deb" nonull
    runcmd "apt-get --assume-yes install -f" nonull
    runcmd "rm -f ${home}/Downloads/insync.deb"
fi

echo_prefix="$echo_prefix_temp"
