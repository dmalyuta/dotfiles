#!/bin/bash
#
# General programming tools.
#
# Author: Danylo Malyuta, 2020.

# ..:: Latest version of Git ::..

GIT_VERSION="2.36.0"
if [ "$(git --version | cut -d ' ' -f 3)" != "$GIT_VERSION" ]; then
    sudo apt-get -y install software-properties-common
    sudo add-apt-repository -y ppa:git-core/ppa
    sudo apt-get update
    sudo apt-get -y install git
fi

# ..:: Misc ::..

sudo apt-get -y install gitk \
    build-essential \
    libgoogle-glog-dev \
    gdbserver \
    aptitude \
    xclip \
    silversearcher-ag \
    screen \
    sshpass \
    tree \
    bash-completion \
    meld \
    kcachegrind \
    curl \
    jq

if not_installed batcat; then
    sudo apt-get -y install bat ripgrep

    # Make the command for bat to be 'bat', not 'batcat'
    mkdir -p ~/.local/bin
    ln -s /usr/bin/batcat ~/.local/bin/bat
fi

# Jsonnet commandline interpreter
sudo apt-get -y install jsonnet

# ..:: System resource monitoring ::..

sudo apt-get -y install htop

if not_installed btm; then
    (cd /tmp &&
        curl -LO https://github.com/ClementTsang/bottom/releases/download/0.6.4/bottom_0.6.4_amd64.deb &&
        sudo dpkg -i bottom_0.6.4_amd64.deb)
fi

if not_installed zenith; then
    wget -4 https://github.com/bvaisvil/zenith/releases/download/0.12.0/zenith_0.12.0-1_amd64.deb -P /tmp/
    (cd /tmp && sudo dpkg -i zenith_0.12.0-1_amd64.deb)
fi

# ..:: Bash ::..

# Bash file formatter. Called by apheleia in Emacs.
if not_installed shfmt; then
    curl -sS https://webi.sh/shfmt | sh
fi

# ..:: Node.js ::..

if not_installed node; then
    # Install Node.js
    (cd /tmp &&
        curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.40.3/install.sh | bash)
    source ~/.bashrc
    nvm install 22.0.0

    # Install typescript and Astro language server
    (cd /tmp &&
        npm install -D typescript &&
        npm i -g @astrojs/language-server)
fi

# ..:: Docker containers ::..

if not_installed docker; then
    # Install prerequisites
    sudo apt-get update
    sudo apt-get -y install apt-transport-https \
        ca-certificates \
        curl \
        gnupg-agent \
        software-properties-common
    # Get Docker repository
    curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo apt-key add -
    if ! sudo apt-key fingerprint 0EBFCD88 | grep -q Docker; then
        echo "Failed to install Docker!"
    else
        sudo add-apt-repository -y \
            "deb [arch=amd64] https://download.docker.com/linux/ubuntu \
             $(lsb_release -cs) \
             stable"
        sudo apt-get update
        # Install Docker
        sudo apt-get install -y docker-ce docker-ce-cli containerd.io
    fi
fi
