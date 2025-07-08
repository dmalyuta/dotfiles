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
    (cd /tmp/ &&
        sudo apt-get -y install ./google-chrome-stable_current_amd64.deb)

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

# ..:: PDF ::..

if not_installed sioyek; then
    # Install qt6 libraries: for main branch.
    sudo apt-get -y install \
        qt5-qmake \
        qtbase5-dev \
        qt3d5-dev \
        libglfw3-dev \
        libfuse3-3 \
        libfuse3-dev \
        libfuse2 \
        libharfbuzz-dev

    # Install qt6 libraries: for development branch.
    #
    # For more flexibility in customization and Qt version, you can skip the
    # apt-get command below and instead install Qt6 community edition from
    # https://www.qt.io/download-qt-installer-oss. Make sure to:
    #  - Select "Custom Installation" in the installer,
    #  - Select Qt -> Qt 6.8.x (e.g. 6.8.3)
    #  - Install all the available Additional Libraries,
    #  - Install Desktop, Sources, and Build Tools -> CMake.
    # Then in the shell do:
    #   export QMAKE=$HOME/Qt/6.8.3/gcc_64/bin/qmake
    # before running build_linux.sh.
    sudo apt-get -y install \
        qt6-base-dev \
        qt6-declarative-dev \
        qt6-svg-dev \
        qt6-speech-dev \
        qt6-quickcontrols2-dev

    # Make qt6 qmake available
    qtchooser -install qt6 /usr/bin/qmake6

    git clone --recursive --branch development \
        https://github.com/ahrm/sioyek /tmp/sioyek
    (
        cd /tmp/sioyek &&
            export QT_SELECT=qt6 &&
            ./build_linux.sh &&
            # If the command below fails, add -unsupported-allow-new-glibc to the
            # end of the line that starts with
            # ./linuxdeployqt-continuous-x86_64.AppImage...
            # (see https://forum.qt.io/post/585565)
            sed -i \
                '/\.\/linuxdeployqt-continuous-x86_64\.AppImage/ {/ -unsupported-allow-new-glibc/! s/$/ -unsupported-allow-new-glibc/}' \
                build_and_release.sh &&
            ./build_and_release.sh &&
            mkdir -p ~/Documents/software &&
            mv sioyek-release $HOME/Documents/software/sioyek &&
            cd $HOME/Documents/software/sioyek &&
            sudo cp usr/share/applications/sioyek.desktop /usr/share/applications/ &&
            sudo cp usr/share/pixmaps/sioyek-icon-linux.png /usr/share/pixmaps/ &&
            sudo ln -sf $HOME/Documents/software/sioyek/usr/bin/sioyek /usr/local/bin/sioyek
    )
fi

# ..:: Enpass password manager ::..

if [[ ! -d /opt/enpass ]]; then
    echo "deb https://apt.enpass.io/ stable main" | sudo tee /etc/apt/sources.list.d/enpass.list
    wget -O - https://apt.enpass.io/keys/enpass-linux.key | sudo apt-key add -
    sudo apt-get -y update
    sudo apt-get -y install enpass
fi

# ..:: Nextcloud cloud storage client ::..
# To run Nextcloud with limited resources (to ensure that it doesn't starve host OS from resources):
#   systemd-run --scope -p MemoryLimit=1000M -p CPUQuota=10% /usr/bin/nextcloud
# See https://www.baeldung.com/linux/limit-resource-consumption

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
    (cd /tmp/textext-1.3.0/ && python3 setup.py)
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
