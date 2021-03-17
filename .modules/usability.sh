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

# ..:: Mouse cursor ::..

mkdir -p ~/.icons

if [ ! -d ~/.icons/GoogleDot ]; then
    wget -4 https://github.com/ful1e5/Google_Cursor/releases/download/v1.0.0/GoogleDot.tar.gz \
	 -O /tmp/GoogleDot.tar.gz
    tar -xvf /tmp/GoogleDot.tar.gz -C /tmp
    mv /tmp/GoogleDot ~/.icons/
fi

# ..:: Gnome desktop customization ::..

# In order to save current extension settings into these dotfiles:
# (source: https://askubuntu.com/a/1178587)
#
#   $ cd <DOTFILES_BASE_DIRECTORY>
#   $ dconf dump /org/gnome/shell/extensions/<EXT_NAME>/ > .config/dconf/<EXT_NAME>.dconf
#
# Note also that you can easily check which dconf settings are changed when
# using GUI, by opening a terminal and running:
#
#   $ dconf watch /
#
# This will print any settings that are changed, every time you change something in GUI.

sudo apt-get -y install dconf-cli

mkdir -p ~/.local/share/gnome-shell/extensions

# Application launchers in the panel
if [ ! -d ~/.local/share/gnome-shell/extensions/arcmenu@arcmenu.com ]; then
    rm -rf /tmp/dash-to-panel
    git clone https://github.com/home-sweet-gnome/dash-to-panel.git /tmp/dash-to-panel
    (cd /tmp/dash-to-panel && make install)

    # Link to my own settings
    dconf load /org/gnome/shell/extensions/dash-to-panel/ < \
	  "$DIR"/.config/dconf/dash-to-panel.dconf
fi

# Traditional menu
if [ ! -d ~/.local/share/gnome-shell/extensions/arcmenu@arcmenu.com ]; then
    rm -rf /tmp/arcmenu
    git clone https://gitlab.com/arcmenu/ArcMenu.git /tmp/arcmenu
    (cd /tmp/arcmenu && make install)

    # Link to my own settings
    dconf load /org/gnome/shell/extensions/arcmenu/ < \
	  "$DIR"/.config/dconf/arcmenu.dconf
fi

# >> Now update the Gnome configuration <<

# Restart the Gnome shell, to make sure everything is up-to-date
busctl --user call org.gnome.Shell /org/gnome/Shell org.gnome.Shell \
       Eval s 'Meta.restart("Restarting Gnome Shell...")'

# Show battery percentage
gsettings set org.gnome.desktop.interface show-battery-percentage true

# Use GoogleDot cursor theme
gsettings set org.gnome.desktop.interface cursor-theme 'GoogleDot'

# Mouse cursor size
gsettings set org.gnome.desktop.interface cursor-size 22

# Enable the extensions
gnome-extensions disable ubuntu-dock@ubuntu.com
gsettings set org.gnome.shell enabled-extensions \
	  "['dash-to-panel@jderose9.github.com', 'arcmenu@arcmenu.com']"

# Use Alt as window action key (e.g. to drag window around)
gsettings set org.gnome.desktop.wm.preferences mouse-button-modifier '<Alt>'

# Detach modal dialogs from parent window (better "Save As" window)
gsettings set org.gnome.mutter attach-modal-dialogs false

# Four static workspaces
gsettings set org.gnome.mutter dynamic-workspaces false
gsettings set org.gnome.desktop.wm.preferences num-workspaces 4

# Turn off natural scrolling
gsettings set org.gnome.desktop.peripherals.mouse natural-scroll false

# ..:: Input device driver ::..

sudo apt-get -y install xserver-xorg-input-all xserver-xorg-input-synaptics
sudo adduser "$USER" input

# ..:: Search ::..

sudo apt-get -y install recoll \
     pdfgrep \
     rofi \
     exo-utils

# ..:: Other ::..

sudo apt-get -y install xcalib \
     compizconfig-settings-manager \
     gnome-tweak-tool \
     pdftk \
     unrar \
     xdotool \
     xbindkeys

# Increase inotify to make sure Evince updates on PDF update
# Source: https://superuser.com/a/1387905/512940
echo fs.inotify.max_user_watches=100000 | sudo tee -a /etc/sysctl.conf
sudo sysctl -p
