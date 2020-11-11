#!/bin/bash
#
# Productivity tools.
#
# Author: Danylo Malyuta, 2020.

# ..:: LaTeX ::..

sudo apt-get -y install texlive-full

# ..:: Inkscape ::..

if not_installed inkscape; then
    sudo add-apt-repository -y ppa:inkscape.dev/stable
    sudo apt-get update
    sudo apt-get -y install inkscape
fi

# ..:: Screen capture ::..

if not_installed shutter; then
    sudo add-apt-repository -y ppa:linuxuprising/shutter
    sudo apt-get update
    sudo apt-get -y install shutter
fi

if not_installed peek; then
    sudo add-apt-repository -y ppa:peek-developers/stable
    sudo apt-get update
    sudo apt-get -y install peek
fi
