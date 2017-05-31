#!/bin/bash
# ----------------------------------------------------------------------
#
# Installation of tools for LaTeX document editing and publication.
#
# ----------------------------------------------------------------------

echo_prefix_temp="$echo_prefix"
echo_prefix="[latex setup] "

apt_get_install_pkg texlive-full nonull
# apt_get_install_pkg texstudio

# Good PDF viewer to use with AUCTeX (Emacs LaTeX mode)
apt_get_install_pkg okular

echo_prefix="$echo_prefix_temp"
