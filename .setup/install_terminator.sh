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
if ! grep -q "parse_return_codes" .bashrc; then
    append_string=true
fi
if ! grep -q "parse_fit_branch" .bashrc; then
    append_string=true
fi
if $append_string; then
    cat <<EOF >> "${home}/.bashrc"
# Add git branch and error code info to bash terminal info string
parse_git_branch() {
    git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'
}
parse_return_code() {
    local last_return_code=$?
    if [ $last_return_code -ne 0 ]; then
	echo " [$last_return_code]"
    else
	echo ""
    fi
}
export PS1="\u@\h \[\033[32m\]\w\[\033[1;31m\]\$(parse_return_code)\[\033[0;33m\]\$(parse_git_branch)\[\033[00m\] $ "
EOF
fi

echo_prefix="$echo_prefix_temp"
