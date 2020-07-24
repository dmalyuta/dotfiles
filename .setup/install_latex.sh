#!/bin/bash
# ----------------------------------------------------------------------
#
# Installation of tools for LaTeX document editing and publication.
#
# ----------------------------------------------------------------------

echo_prefix_temp="$echo_prefix"
echo_prefix="[latex setup] "

apt_get_install_pkg texlive-full nonull

# # Good PDF viewer to use for forward/backward searching
# # with e.g. TeXlipse
# apt_get_install_pkg okular

echo_prefix="$echo_prefix_temp"
