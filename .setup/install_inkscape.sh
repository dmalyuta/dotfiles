#!/bin/bash
# ----------------------------------------------------------------------
#
# Installation of Inkscape drawing tool.
#
# ----------------------------------------------------------------------

echo_prefix_temp="$echo_prefix"
echo_prefix="[inkscape setup] "

if program_not_installed "inkscape"; then
    runcmd "add-apt-repository ppa:inkscape.dev/stable -y"
    runcmd "apt-get update"
    apt_get_install_pkg inkscape
fi

echo_prefix="$echo_prefix_temp"
