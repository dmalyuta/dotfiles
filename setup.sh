#!/bin/bash
# ----------------------------------------------------------------------
#
# Main setup script.
# You can call it as follows:
#   ./setup.sh setup <opts>
#   ./setup.sh symlink <opts>
# where you can use -? to see help for <opts>
#
# ----------------------------------------------------------------------

mode="$1"
normal_user="${SUDO_USER:-$USER}"
base_cmd="sudo --preserve-env=HOME,PATH" #-u $normal_user"

if [ "$mode" == "setup" ]; then
    $base_cmd ./setup_new_machine.sh "${@:2}"
    $base_cmd ./symlink_update.sh "${@:2}"
elif [ "$mode" == "symlink" ]; then
    $base_cmd ./symlink_update.sh "${@:2}"
fi
