#!/bin/bash
#
# Hardware drivers and peripherals.
#
# Author: Danylo Malyuta, 2022.

# ..:: Logitech ::..

# Unifying receiver software
sudo apt-get -y install solaar

# Logitech drivers
# https://danishshakeel.me/configure-logitech-mx-master-3-on-linux-logiops/
if not_installed logid; then
    sudo apt-get -y install cmake libevdev-dev libudev-dev libconfig++-dev

    git clone https://github.com/PixlOne/logiops.git /tmp/logiops
    ( cd /tmp/logiops/ && \
        mkdir build && \
        cd build && \
        cmake .. && \
        make && \
        sudo make install )

    sudo systemctl enable --now logid
fi
