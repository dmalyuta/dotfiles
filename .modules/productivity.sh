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

# ..:: Logseq (note taking in markdown) ::..

if [[ ! -f ~/.local/bin/logseq.AppImage ]]; then
    LOGSEQ_VERSION=0.7.1
    wget -4 https://github.com/logseq/logseq/releases/download/$LOGSEQ_VERSION/logseq-linux-x64-$LOGSEQ_VERSION.AppImage \
        -O /tmp/logseq.AppImage
    sudo chmod +x /tmp/logseq.AppImage
    sudo mv /tmp/logseq.AppImage /usr/bin/logseq

    # Create the desktop entry to launch the app.
    wget -4 https://raw.githubusercontent.com/logseq/logseq/master/resources/img/logo.png \
        -O /tmp/Logseq.png
    sudo cp /tmp/Logseq.png /usr/share/pixmaps/Logseq.png
    sudo desktop-file-install "$DIR"/.local/share/applications/logseq.desktop
    sudo update-desktop-database
fi

# ..:: Obsidian (note taking in markdown) ::..

if not_installed obsidian; then
    OBSIDIAN_VERSION=0.14.6
    wget https://github.com/obsidianmd/obsidian-releases/releases/download/v$OBSIDIAN_VERSION/obsidian_$OBSIDIAN_VERSION_amd64.snap \
        -O /tmp/obsidian.snap
    sudo snap install --dangerous /tmp/obsidian.snap

    # Fonts for appearance
    wget https://icomoon.io/LigatureFont.zip -O /tmp/LigatureFont.zip
    unzip -o /tmp/LigatureFont.zip -d /tmp/
    mkdir -p ~/.local/share/fonts/
    cp /tmp/Font/IcoMoon-Free.ttf ~/.local/share/fonts/
    fc-cache -f -v
    if ! (fc-list | grep -q IcoMoon); then
        echo '/!\ Failed to install IcoMoon font.'
    fi
fi

# ..:: Micropad ::..

if not_installed micropad; then
    sudo snap install micropad
fi

# ..:: Enpass password manager ::..

if [[ ! -d /opt/enpass ]]; then
    echo "deb https://apt.enpass.io/ stable main" | sudo tee /etc/apt/sources.list.d/enpass.list
    wget -O - https://apt.enpass.io/keys/enpass-linux.key | sudo apt-key add -
    sudo apt-get -y update
    sudo apt-get -y install enpass
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

# ..:: Draw.io ::..

if not_installed drawio; then
    DRAWIO_VERSION=17.4.2
    wget https://github.com/jgraph/drawio-desktop/releases/download/v$DRAWIO_VERSION/drawio-amd64-$DRAWIO_VERSION.deb \
        -O /tmp/drawio.deb
    sudo apt-get install -y /tmp/drawio.deb
fi

# ..:: Screen capture ::..

sudo apt-get -y install shutter
sudo apt-get -y install flameshot

if not_installed peek; then
    sudo add-apt-repository -y ppa:peek-developers/stable
    sudo apt-get update
    sudo apt-get -y install peek
fi

# ..:: OS image ISO flasher ::..

if [[ ! -d /opt/balenaEtcher ]]; then
    curl -1sLf 'https://dl.cloudsmith.io/public/balena/etcher/setup.deb.sh' | sudo -E bash
    sudo apt-get -y update
    sudo apt-get -y install balena-etcher-electron
fi

# ..:: Miscellaneous ::..

# Infinite canvas for reference images.
sudo apt-get -y install pureref

# ..:: Command line ::..

if not_installed fzf; then
    git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
    ~/.fzf/install --all
fi

# ..:: Gedit plugins ::..

GEDIT_PLUGINS_DIR=~/.local/share/gedit/plugins/

mkdir -p $GEDIT_PLUGINS_DIR

SCROLL_PAST_DIR="${GEDIT_PLUGINS_DIR}scroll-past"
if [[ ! -d "$SCROLL_PAST_DIR" ]]; then
    git clone https://github.com/hardpixel/gedit-scroll-past "$SCROLL_PAST_DIR"
fi

# Set default settings
cat "$DIR"/.config/gedit_settings.txt | xargs -L 1 gsettings set
