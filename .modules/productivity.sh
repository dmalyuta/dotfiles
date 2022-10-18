#!/bin/bash
#
# Productivity tools.
#
# Author: Danylo Malyuta, 2020.

# ..:: Chrome browser ::..

# Remove Firefox
if ! not_installed firefox; then
    sudo snap remove --purge firefox
fi

if not_installed google-chrome; then
    sudo apt-get -y install apt-transport-https gnupg
    wget https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb -P /tmp/
    ( cd /tmp/ && \
          sudo apt-get -y install ./google-chrome-stable_current_amd64.deb )

    # Make `browser` command launch Chrome.
    sudo ln -sf /usr/bin/google-chrome /usr/bin/browser
fi

# ..:: LaTeX ::..

sudo apt-get -y install texlive-full

# ..:: LibreOffice ::..

if ! (sudo ls -1 /etc/apt/sources.list.d/ | grep -q "libreoffice"); then
    sudo add-apt-repository -y ppa:libreoffice/ppa
fi

if not_installed libreoffice || ! (libreoffice --version | grep -Eq ".*7.*"); then
    sudo apt-get -y purge libreoffice*
    sudo apt-get -y clean
    sudo apt-get -y autoremove
    sudo apt-get -y update
    sudo apt-get -y dist-upgrade
    sudo apt-get -y install libreoffice
fi

# ..:: Enpass password manager ::..

if [[ ! -d /opt/enpass ]]; then
    echo "deb https://apt.enpass.io/ stable main" | sudo tee /etc/apt/sources.list.d/enpass.list
    wget -O - https://apt.enpass.io/keys/enpass-linux.key | sudo apt-key add -
    sudo apt-get -y update
    sudo apt-get -y install enpass
fi

# ..:: Nextcloud cloud storage client ::..

if not_installed nextcloud; then
    sudo add-apt-repository -y ppa:nextcloud-devs/client
    sudo apt-get -y update
    sudo apt-get -y install nextcloud-client
fi

# ..:: Inkscape ::..

if not_installed inkscape; then
    sudo add-apt-repository -y universe
    sudo add-apt-repository -y ppa:inkscape.dev/trunk
    sudo apt-get update
    sudo apt-get -y --install-suggests install inkscape-trunk
fi

# Install TeX Text plugin for better LaTeX editing
# Find it under [Menu Bar -> Extensions -> Text -> Tex Text]
# https://github.com/textext/textext
if [[ ! -d ~/.config/inkscape/extensions/textext ]]; then
    sudo apt-get -y install gir1.2-gtksource-3.0
    wget -4 https://github.com/textext/textext/releases/download/1.3.0/TexText-Linux-1.3.0.tar.gz -P /tmp/
    (cd /tmp/ && tar -zxvf /tmp/TexText-Linux-1.3.0.tar.gz ./textext-1.3.0)
    ( cd /tmp/textext-1.3.0/ && python3 setup.py )
fi

# ..:: Screen capture ::..

sudo apt-get -y install shutter
sudo apt-get -y install flameshot

if not_installed peek; then
    sudo add-apt-repository -y ppa:peek-developers/stable
    sudo apt-get update
    sudo apt-get -y install peek
fi

# ..:: Command line ::..

if not_installed fzf; then
    git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
    ~/.fzf/install --all
fi
