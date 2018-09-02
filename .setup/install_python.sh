#!/bin/bash
# ----------------------------------------------------------------------
#
# Installation of tools for better Python programming experience.
#
# ----------------------------------------------------------------------

echo_prefix_temp="$echo_prefix"
echo_prefix="[python tools setup] "

# install fresh pip
apt_get_install_pkg python-dev
apt_get_install_pkg python-pip
apt_get_install_pkg python3-dev
apt_get_install_pkg python3-pip
runcmd "sudo -H python -m pip install --upgrade --force pip"

########################
# Anaconda
########################

runcmd "wget https://repo.anaconda.com/archive/Anaconda2-5.2.0-Linux-x86_64.sh -O /tmp/anaconda.sh"
runcmd_noexit "chmod +x /tmp/anaconda.sh" nonull
runcmd_noexit "/tmp/anaconda.sh" nonull

echo_prefix="$echo_prefix_temp"
