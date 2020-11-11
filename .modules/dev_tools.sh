#!/bin/bash
#
# General programming tools.
#
# Author: Danylo Malyuta, 2020.

# Latest version of Git
GIT_VERSION="$(git --version | cut -d ' ' -f 3 | cut -d '.' -f 1)"
if [ "$GIT_VERSION" -lt 2 ]; then
    sudo apt-get -y install python-software-properties
    sudo add-apt-repository -y ppa:git-core/ppa
    sudo apt-get update
    sudo apt-get -y install git
fi

sudo apt-get -y install gitk \
     build-essential \
     libgoogle-glog-dev \
     gdbserver \
     aptitude \
     xclip \
     silversearcher-ag \
     screen \
     htop \
     sshpass \
     tree \
     bash-completion \
     meld

if function_not_defined cmake; then
    wget -4 https://github.com/Kitware/CMake/releases/download/v3.18.4/cmake-3.18.4.tar.gz \
	 -O /tmp/cmake.tar.gz
    tar -xvf /tmp/cmake.tar.gz -C /tmp
    ( cd /tmp/cmake/ && ./bootstrap && make && sudo make install )
fi
