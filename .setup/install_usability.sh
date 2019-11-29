#!/bin/bash
# ----------------------------------------------------------------------
#
# Install usability-related things for Linux like the theme, the text
# editor, etc.
#
# ----------------------------------------------------------------------

echo_prefix_temp="$echo_prefix"
echo_prefix="[usability setup] "

# Numix theme & icon set
# https://github.com/numixproject
if ! dpkg-query -W numix-gtk-theme &>/dev/null; then
    # Install if not already installed
    echowarn "Afterwards, run GNOME Tweaks and set the Icons-->Numix-Circle!"
    apt_get_install_pkg gnome-tweak-tool
    apt_get_install_pkg numix-gtk-theme
    runcmd "gsettings set org.gnome.desktop.interface gtk-theme \"Numix\""
    runcmd "gsettings set org.gnome.desktop.wm.preferences theme \"Numix\""
    runcmd "add-apt-repository ppa:numix/ppa -y"
    runcmd "apt-get update"
    apt_get_install_pkg numix-icon-theme
    apt_get_install_pkg numix-icon-theme-circle
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

# Gnome pixel saver
# https://github.com/pixel-saver/pixel-saver
# Merges top bar and window title bar when maximized
apt_get_install_pkg gnome-shell-extension-pixelsaver
gnome-shell-extension-tool -e pixel-saver@deadalnix.me
# For code changes to become effective, you might need to reload GNOME Shell by
# pressing Alt+F2 and entering r

echo_prefix="$echo_prefix_temp"
