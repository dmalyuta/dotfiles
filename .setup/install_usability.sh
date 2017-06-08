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
    runcmd "wget https://download.sublimetext.com/sublime-text_build-3126_amd64.deb -O /tmp/sublime_text.deb"
    runcmd_noexit "dpkg -i /tmp/sublime_text.deb" nonull
    runcmd "apt-get --assume-yes install -f" nonull
fi

# LibreOffice 5.x
# Office Suite
install_libreoffice=0
if program_not_installed "libreoffice"; then
    install_libreoffice=1
else
    libreoffice_version="$(libreoffice --version | cut -d ' ' -f 2 | cut -d '.' -f 1)"
    if [ "$libreoffice_version" -lt 5 ]; then
        # Need to upgrade current libreoffice
        install_libreoffice=1
    fi
fi
if [ "$install_libreoffice" -eq 1 ]; then
    runcmd "add-apt-repository ppa:libreoffice/ppa -y"
    runcmd "apt-get update"
    apt_get_install_pkg libreoffice
fi

# Byzanz
# Screencast GIF recorder
# Run .bin/byzanz-gui script for GUI
apt_get_install_pkg byzanz

# Mendeley
# Research bibliography organizer
if program_not_installed "mendeleydesktop"; then
    runcmd "wget https://www.mendeley.com/repositories/ubuntu/stable/amd64/mendeleydesktop-latest -O /tmp/mendeley.deb"
    runcmd_noexit "dpkg -i /tmp/mendeley.deb" nonull
    runcmd "apt-get --assume-yes install -f" nonull
fi


echo_prefix="$echo_prefix_temp"
