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

# update .bashrc to include an indication of which Git branch user is on
source_local_bashrc=false
if ! grep -q ".local.bashrc" "${home}/.bashrc"; then
    source_local_bashrc=true
fi
if $source_local_bashrc; then
    # source .local.bashrc
    runcmd "eval builtin echo \"\" >> ${home}/.profile" nonull
    runcmd "eval builtin echo \"# personal additions to .bashrc\" >> ${home}/.profile" nonull
    runcmd "eval builtin echo \"if [[ -f ~/.local.bashrc ]]; then\" >> ${home}/.profile" nonull
    runcmd "eval builtin echo \"    source .local.bashrc\" >> ${home}/.profile" nonull
    runcmd "eval builtin echo \"fi\" >> ${home}/.profile" nonull
fi

echo_prefix="$echo_prefix_temp"
