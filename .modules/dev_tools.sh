#!/bin/bash
#
# General programming tools.
#
# Author: Danylo Malyuta, 2020.

# ..:: Latest version of Git ::..

GIT_VERSION="$(git --version | cut -d ' ' -f 3 | cut -d '.' -f 1)"
if [ "$GIT_VERSION" -lt 2 ]; then
    sudo apt-get -y install python-software-properties
    sudo add-apt-repository -y ppa:git-core/ppa
    sudo apt-get update
    sudo apt-get -y install git
fi

# Git GUI
sudo apt-get -y install gitg

# ..:: Misc ::..

sudo apt-get -y install gitk \
    build-essential \
    libgoogle-glog-dev \
    gdbserver \
    aptitude \
    xclip \
    silversearcher-ag \
    screen \
    tmux \
    sshpass \
    tree \
    bash-completion \
    meld \
    kcachegrind \
    curl

if not_installed batcat; then
    sudo apt-get -y install -o Dpkg::Options::=--force-overwrite bat ripgrep

    # Make the command for bat to be 'bat', not 'batcat'
    sudo dpkg-divert --package batcat --add --rename --divert /usr/bin/bat /usr/bin/batcat
fi

# Jsonnet commandline interpreter
sudo apt-get -y install jsonnet

# ..:: System resource monitoring ::..

sudo apt-get -y install htop

if not_installed btm; then
    ( cd /tmp &&
      curl -LO https://github.com/ClementTsang/bottom/releases/download/0.6.4/bottom_0.6.4_amd64.deb &&
      sudo dpkg -i bottom_0.6.4_amd64.deb )
fi

if not_installed zenith; then
    wget -4 https://github.com/bvaisvil/zenith/releases/download/0.12.0/zenith_0.12.0-1_amd64.deb -P /tmp/
    ( cd /tmp && sudo dpkg -i zenith_0.12.0-1_amd64.deb )
fi

# ..:: Node.js ::..

if not_installed node; then
    curl -sL https://deb.nodesource.com/setup_12.x | sudo -E bash -
    sudo apt-get -y install nodejs
fi

# ..:: CMake ::..

if not_installed cmake; then
    # Dependencies
    sudo apt-get -y install libssl-dev

    wget -4 https://github.com/Kitware/CMake/releases/download/v3.18.4/cmake-3.18.4.tar.gz -P /tmp/
    tar -xvf /tmp/cmake-3.18.4.tar.gz -C /tmp
    ( cd /tmp/cmake-3.18.4/ && ./bootstrap && make && sudo make install )
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
	sudo add-apt-repository \
	     "deb [arch=amd64] https://download.docker.com/linux/ubuntu \
             $(lsb_release -cs) \
             stable"
	sudo apt-get update
	# Install Docker
	sudo apt-get install docker-ce docker-ce-cli containerd.io
    fi
fi
