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
