#!/bin/bash
# ----------------------------------------------------------------------
#
# Installation of creative applications for drawing and diagramming.
#
# ----------------------------------------------------------------------

echo_prefix_temp="$echo_prefix"
echo_prefix="[creative setup] "

# Inkscape
# Vector graphics
if program_not_installed "inkscape"; then
    runcmd "add-apt-repository ppa:inkscape.dev/stable -y"
    runcmd "apt-get update"
    apt_get_install_pkg inkscape
fi

# Kazam
# Screen recorder
apt_get_install_pkg kazam

# Peek
# GIF screen recorder
if program_not_installed "peek"; then
    runcmd "add-apt-repository ppa:peek-developers/stable -y"
    runcmd "apt-get update"
    apt_get_install_pkg peek
fi

echo_prefix="$echo_prefix_temp"
