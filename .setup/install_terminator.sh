#!/bin/bash
# ----------------------------------------------------------------------
#
# Terminator installation.
#
# A terminal emulator supporting tabs and multiple resizable terminal
# panels in one window native based on GNOME Terminal
#
# To learn more about Terminator, visit:
# https://gnometerminator.blogspot.ch/p/introduction.html
#
# ----------------------------------------------------------------------

echo_prefix_temp="$echo_prefix"
echo_prefix="[terminator setup] "

# install terminator

apt_get_install_pkg terminator

# remove mate terminal

runcmd "apt-get --assume-yes purge mate-terminal"

echo_prefix="$echo_prefix_temp"
