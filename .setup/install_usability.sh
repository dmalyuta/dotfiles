#!/bin/bash
# ----------------------------------------------------------------------
#
# Install usability-related things for Linux like the theme.
#
# ----------------------------------------------------------------------

echo_prefix_temp="$echo_prefix"
echo_prefix="[usability setup] "

# Flatabulous theme & icon set
# https://github.com/anmoljagetia/Flatabulous
if ! dpkg-query -W flatabulous-theme &>/dev/null; then
    # Install if not already installed
    echowarn "Afterwards, run Unity Tweak Tool and set the Theme-->Flatabulous, Icons-->Ultra-flat!"
    apt_get_install_pkg unity-tweak-tool
    runcmd "add-apt-repository ppa:noobslab/themes -y"
    runcmd "add-apt-repository ppa:noobslab/icons -y"
    runcmd "apt-get update"
    apt_get_install_pkg flatabulous-theme
    apt_get_install_pkg ultra-flat-icons
fi



echo_prefix="$echo_prefix_temp"
