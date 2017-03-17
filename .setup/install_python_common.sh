#!/bin/bash
# ----------------------------------------------------------------------
#
# Installation of tools for better Python programming experience.
#
# ----------------------------------------------------------------------

echo_prefix_temp="$echo_prefix"
echo_prefix="[python dependencies setup] "

# install fresh pip
apt_get_install_pkg python-dev
apt_get_install_pkg python-pip
apt_get_install_pkg python3-dev
apt_get_install_pkg python3-pip
runcmd "sudo -H python -m pip install --upgrade --force pip"
runcmd "sudo -H pip install setuptools==33.1.1"
# runcmd "eval curl https://bootstrap.pypa.io/get-pip.py | python"

echo_prefix="$echo_prefix_temp"
