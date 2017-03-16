#!/bin/bash
# ----------------------------------------------------------------------
#
# Installation of Inkscape drawing tool.
#
# ----------------------------------------------------------------------

echo_prefix_temp="$echo_prefix"
echo_prefix="[inkscape setup] "

runcmd "add-apt-repository ppa:inkscape.dev/stable -y"
runcmd "apt-get update"
apt_get_install_pkg inkscape

echo_prefix="$echo_prefix_temp"
