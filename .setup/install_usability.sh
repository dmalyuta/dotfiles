#!/bin/bash
# ----------------------------------------------------------------------
#
# Install usability-related things for Linux like the theme, the text
# editor, etc.
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

# Mendeley
# Research bibliography organizer
if program_not_installed "mendeleydesktop"; then
    runcmd "wget https://www.mendeley.com/repositories/ubuntu/stable/amd64/mendeleydesktop-latest -O /tmp/mendeley.deb"
    runcmd_noexit "dpkg -i /tmp/mendeley.deb" nonull
    runcmd "apt-get --assume-yes install -f" nonull
fi

# Autorandr
# Automatic detection and changing of display configuration
runcmd "sudo -H pip install autorandr"

# Compiz
# Graphics/desktop customization program
apt_get_install_pkg compizconfig-settings-manager

# Synapse
# Instant search
if program_not_installed "synapse"; then
    runcmd "add-apt-repository ppa:synapse-core/testing -y"
    runcmd "apt-get update"
    apt_get_install_pkg synapse
fi

echo_prefix="$echo_prefix_temp"
