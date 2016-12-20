#!/bin/bash
# ----------------------------------------------------------------------
#
# Google Chrome installation.
#
# A fast, secure, and free web browser built for the modern
# web. Attention! Chrome is based an Chromium, an open-source project,
# but itself contains additional non-free (free as in “free speech”,
# not as in "free beer") elements.
#
# To learn more about Google Chrome, visit:
# https://www.google.com/chrome/browser/desktop/
#
# ----------------------------------------------------------------------

echo_prefix_temp="$echo_prefix"
echo_prefix="[chrome setup] "

if program_not_installed "google-chrome"; then
    # install Chrome's dependencies
    apt_get_install_pkg libxss1
    apt_get_install_pkg libappindicator1
    apt_get_install_pkg libindicator7

    # install Chrome itself
    runcmd "wget https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb -P ${home}/Downloads/"
    runcmd "dpkg -i ${home}/Downloads/google-chrome*.deb"

    # remove install file
    runcmd "rm -rf ${home}/Downloads/google-chrome*.deb"
fi

echo_prefix="$echo_prefix_temp"
