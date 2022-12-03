#!/bin/bash
#
# Hardware drivers and peripherals.
#
# Author: Danylo Malyuta, 2022.

# ..:: Logitech ::..

# Unifying receiver software
# https://pwr-solaar.github.io/Solaar/installation
if not_installed solaar; then
    rm -rf /tmp/solaar
    git clone https://github.com/pwr-Solaar/Solaar.git /tmp/solaar
    ( cd /tmp/solaar && \
        sudo cp rules.d/42-logitech-unify-permissions.rules /etc/udev/rules.d && \
        sudo udevadm control --reload-rules && \
        sudo bash -c 'umask 022 ; pip install .' )
fi

# Logitech drivers
# https://danishshakeel.me/configure-logitech-mx-master-3-on-linux-logiops/
if not_installed logid; then
    sudo apt-get -y install cmake libevdev-dev libudev-dev libconfig++-dev

    rm -rf /tmp/logiops
    git clone https://github.com/PixlOne/logiops.git /tmp/logiops
    ( cd /tmp/logiops/ && \
        mkdir build && \
        cd build && \
        cmake .. && \
        make && \
        sudo make install )

    sudo systemctl enable --now logid
fi
