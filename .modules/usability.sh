#!/bin/bash
#
# General usability tools and theming.
#
# Author: Danylo Malyuta, 2020.

# ..:: Terminal emulator ::..

if not_installed terminator; then
    sudo apt-get -y install terminator

    # Make Terminator the new Gnome Terminal!
    sudo apt-get purge -y gnome-terminal
    sudo ln -s /usr/bin/terminator /usr/bin/gnome-terminal
fi

# Fix bug that Ctrl-Alt-T creates a new icon sidebar
gsettings set org.gnome.desktop.default-applications.terminal exec "terminator"

# Show battery percentage
gsettings set org.gnome.desktop.interface show-battery-percentage true

# ..:: Mouse cursor ::..

mkdir -p ~/.icons

if [ ! -d ~/.icons/GoogleDot ]; then
    wget -4 https://github.com/ful1e5/Google_Cursor/releases/download/v1.0.0/GoogleDot.tar.gz \
	 -O /tmp/GoogleDot.tar.gz
    tar -xvf /tmp/GoogleDot.tar.gz -C /tmp
    mv /tmp/GoogleDot ~/.icons/
fi

# ..:: Input device driver ::..

sudo apt-get -y install xserver-xorg-input-all xserver-xorg-input-synaptics
sudo adduser "$USER" input

# ..:: Search ::..

sudo apt-get -y install recoll \
     pdfgrep \
     rofi \
     exo-utils

# ..:: i3 window manager ::..

if not_installed i3; then
    # Basic tools
    sudo apt-get -y install i3 \
	 i3status \
	 i3lock \
	 i3blocks \
	 rofi \
	 feh \
	 sysstat \
	 acpi \
	 compton

    # Gaps in windows
    sudo add-apt-repository -y ppa:kgilmer/speed-ricer
    sudo apt-get update
    sudo apt-get -y install i3-gaps
fi

# ..:: Other ::..

sudo apt-get -y install xcalib \
     compizconfig-settings-manager \
     gnome-tweak-tool \
     pdftk \
     unrar

# Increase inotify to make sure Evince updates on PDF update
# Source: https://superuser.com/a/1387905/512940
echo fs.inotify.max_user_watches=100000 | sudo tee -a /etc/sysctl.conf
sudo sysctl -p
