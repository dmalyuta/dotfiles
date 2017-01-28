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
append_string=false
if ! grep -q "parse_return_codes" "${home}/.bashrc"; then
    append_string=true
fi
if ! grep -q "parse_fit_branch" "${home}/.bashrc"; then
    append_string=true
fi
if $append_string; then
    # append this functionality to the .bashrc file
    runcmd "eval cat ${dir}/.bashrc_additions/bash_prompt >> ${home}/.bashrc" nonull
fi

echo_prefix="$echo_prefix_temp"
