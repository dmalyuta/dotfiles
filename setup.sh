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

if [ "$mode" == "setup" ]; then
    sudo --preserve-env=HOME,PATH ./setup_new_machine.sh "${@:2}"
    sudo --preserve-env=HOME,PATH ./symlink_update.sh "${@:2}"
elif [ "$mode" == "symlink" ]; then
    sudo --preserve-env=HOME,PATH ./symlink_update.sh "${@:2}"
fi
