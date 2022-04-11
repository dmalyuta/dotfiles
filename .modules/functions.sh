#!/bin/bash
#
# Common utility functions.
#
# Author: Danylo Malyuta, 2020.

not_installed() {
    # Returns true if input is not defined
    local name="$1"
    ! type -t "$name" > /dev/null 2>&1
}

parse_yn() {
    if [[ "$1" == "" || "$1" == "Y" || "$1" == "y" ]]; then
        true
    else
        false
    fi
}

update_bashrc() {
    if ! grep -q ".local.bashrc" ~/.bashrc; then
        cat << EOF >> ~/.bashrc
#############################################
#### This part should go at the very end ####
#############################################
export PATH=\$PATH:~/.bin/git-custom-commands
if [ -f ~/.local.bashrc ]; then
    . ~/.local.bashrc
fi
EOF
    fi
}
