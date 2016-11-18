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

# remove Firefox

runcmd "apt-get --assume-yes purge firefox"

# install Chrome's dependencies

apt_get_install_pkg libxss1
apt_get_install_pkg libappindicator1
apt_get_install_pkg libindicator7

# Chrome itself

if ! google-chrome --version >/dev/null 2>&1; then
    runcmd "wget https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb"
    runcmd "dpkg -i google-chrome*.deb"
    runcmd "rm -rf google-chrome*.deb"
fi

echo_prefix="$echo_prefix_temp"
