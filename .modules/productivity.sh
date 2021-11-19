#!/bin/bash
#
# Productivity tools.
#
# Author: Danylo Malyuta, 2020.

# ..:: Brave Browser ::..

# Remove Firefox
if ! not_installed firefox; then
    sudo apt-get -y purge firefox
    sudo apt-get -y autoremove
    rm -rf .mozilla/firefox/
    sudo rm -rf /etc/firefox/
    sudo rm -rf /usr/lib/firefox-addons
fi

if not_installed brave-browser; then
    sudo apt-get -y install apt-transport-https curl gnupg
    curl -s https://brave-browser-apt-release.s3.brave.com/brave-core.asc | \
	sudo apt-key --keyring /etc/apt/trusted.gpg.d/brave-browser-release.gpg add -
    echo "deb [arch=amd64] https://brave-browser-apt-release.s3.brave.com/ stable main" | \
	sudo tee /etc/apt/sources.list.d/brave-browser-release.list
    sudo apt-get -y update
    sudo apt-get -y install brave-browser
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

# ..:: Logseq (note taking in markdown) ::..

if [[ ! -f ~/.local/bin/logseq.AppImage ]]; then
    mkdir -p ~/.local/bin/
    wget -4 https://github.com/logseq/logseq/releases/download/0.4.9/logseq-linux-x64-0.4.9.AppImage \
        -O ~/.local/bin/logseq.AppImage
    sudo chmod +x ~/.local/bin/logseq.AppImage
fi

# ..:: Enpass password manager ::..

if [[ ! -d /opt/enpass ]]; then
    echo "deb https://apt.enpass.io/ stable main" | \
	sudo tee /etc/apt/sources.list.d/enpass.list
    wget -O - https://apt.enpass.io/keys/enpass-linux.key | sudo apt-key add -
    sudo apt-get -y update
    sudo apt-get -y install enpass
fi

# ..:: Inkscape ::..

if not_installed inkscape; then
    sudo add-apt-repository -y ppa:inkscape.dev/stable
    sudo apt-get update
    sudo apt-get -y install inkscape
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

if not_installed shutter; then
    sudo add-apt-repository -y ppa:linuxuprising/shutter
    sudo apt-get update
    sudo apt-get -y install shutter
fi

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

# ..:: Gedit plugins ::..

GEDIT_PLUGINS_DIR=~/.local/share/gedit/plugins/

mkdir -p GEDIT_PLUGINS_DIR

SCROLL_PAST_DIR="${GEDIT_PLUGINS_DIR}scroll-past"
if [[ ! -d "$SCROLL_PAST_DIR" ]]; then
    git clone https://github.com/hardpixel/gedit-scroll-past "$SCROLL_PAST_DIR"
fi

# Set default settings
cat "$DIR"/.config/gedit_settings.txt | xargs -L 1 gsettings set
