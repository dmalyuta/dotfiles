#!/bin/bash
# ----------------------------------------------------------------------
#
# Git installation.
#
# Git is a free and open source distributed version control system
# designed to handle everything from small to very large projects with
# speed and efficiency.
#
# To learn more about Git, visit: https://git-scm.com/
#
# ----------------------------------------------------------------------

echo_prefix_temp="$echo_prefix"
echo_prefix="[git setup] "

apt_get_install_pkg git

echo_prefix="$echo_prefix_temp"
