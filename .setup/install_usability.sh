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

# Sublime Text 3
# Text editor
if program_not_installed "subl"; then
    # Install if not already installed
    runcmd "wget https://download.sublimetext.com/sublime-text_build-3126_amd64.deb -O ${home}/Downloads/sublime_text.deb"
    runcmd_noexit "dpkg -i ${home}/Downloads/sublime_text.deb" nonull
    runcmd "apt-get --assume-yes install -f" nonull
    runcmd "rm -f ${home}/Downloads/sublime_text.deb"
fi




echo_prefix="$echo_prefix_temp"
