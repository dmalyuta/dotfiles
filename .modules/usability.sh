#!/bin/bash
#
# General usability tools and theming.
#
# Author: Danylo Malyuta, 2020.

# ..:: Shell type ::..

sudo usermod -s /bin/bash "$USERNAME"

# ..:: Terminal emulator ::..

if not_installed kitty; then
    # Instructions: https://sw.kovidgoyal.net/kitty/binary/
    curl -L https://sw.kovidgoyal.net/kitty/installer.sh | sh /dev/stdin launch=n

    # Create desktop entry for Kitty
    ln -s ~/.local/kitty.app/bin/kitty ~/.local/bin/
    cp ~/.local/kitty.app/share/applications/kitty.desktop ~/.local/share/applications/
    cp ~/.local/kitty.app/share/applications/kitty-open.desktop ~/.local/share/applications/
    sed -i "s|Icon=kitty|Icon=/home/$USER/.local/kitty.app/share/icons/hicolor/256x256/apps/kitty.png|g" \
        ~/.local/share/applications/kitty*.desktop
    sed -i "s|Exec=kitty|Exec=/home/$USER/.local/kitty.app/bin/kitty|g" \
        ~/.local/share/applications/kitty*.desktop
    sudo desktop-file-install ~/.local/share/applications/kitty*.desktop
    sudo update-desktop-database

    # Make Kitty the new terminal!
    if ! not_installed kitty; then
        sudo update-alternatives --install /usr/bin/x-terminal-emulator x-terminal-emulator ~/.local/bin/kitty 100
        sudo update-alternatives --set x-terminal-emulator ~/.local/bin/kitty
    fi
fi

# ..:: Tmux ::..
# Terminal multiplexer

if not_installed tmux; then
    # Install needed packages
    sudo apt-get install -y bison \
        libevent-dev \
        libncurses-dev \
        autotools-dev \
        automake

    git clone https://github.com/tmux/tmux.git /tmp/tmux
    ( cd /tmp/tmux && \
      sh autogen.sh && \
      ./configure && \
      make && \
      sudo make install )
fi

if [ ! -d ~/.tmux/plugins/tpm ]; then
    # Tmux Plugin Manager
    mkdir -p ~/.tmux/plugins
    git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
fi

# ..:: Search ::..

sudo apt-get -y install recoll \
     pdfgrep \
     exo-utils \
     tree \
     fd-find

# ..:: Wine ::..
# Windows program emulation

if not_installed wine; then
    # Instructions:
    # https://www.omgubuntu.co.uk/2021/01/wine-6-0-released-how-to-install-on-ubuntu
    # https://linuxize.com/post/how-to-install-wine-on-ubuntu-20-04/
    # https://askubuntu.com/questions/219791/improve-gui-appearance-of-wine-applications

    wget -qO- https://dl.winehq.org/wine-builds/winehq.key | sudo apt-key add -
    sudo add-apt-repository -y 'deb https://dl.winehq.org/wine-builds/ubuntu/ focal main'
    sudo apt-get -y --install-recommends install winehq-staging
    sudo apt-get -y install winetricks

    # Install fonts
    winetricks allfonts
fi

# ..:: Other ::..

# Disable the message "Sorry, Ubuntu xy.zw has experienced an internal error"
sudo sed -i -e '/enabled=/s/1/0/' /etc/default/apport

sudo apt-get -y install xcalib \
     pdftk \
     unrar \
     xdotool \
     simple-scan

# Increase inotify to make sure Evince updates on PDF update
# Source: https://superuser.com/a/1387905/512940
echo fs.inotify.max_user_watches=524288 | sudo tee -a /etc/sysctl.conf
sudo sysctl -p
