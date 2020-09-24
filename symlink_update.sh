#!/bin/bash
# ----------------------------------------------------------------------
#
# Verify and update symlinks of $HOME dotfiles to this Git
# repository's dotfiles. Use-case: use has moved the this Git
# repository's folder to a different folder, now symlinks of $HOME
# dotfiles are broken
#
# Run as root using: sudo --preserve-env=HOME
# (to preserve the home directory of the user, necessary for 
#  Ubuntu >=19.10)
#
# ----------------------------------------------------------------------

dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )" # directory where symlink_update.sh located
cd "$dir"

home="${HOME}" # home directory
dryrun=false
symlink=false
green="\e[0;32m"
cyanbold="\e[1;36m"
yellowbold="\e[1;33m"
redbold="\e[1;31m"
nostyle="\e[0m"
echo_prefix="[default] "
number_warnings=0
number_errors=0

########## functions

source .setup/functions.sh

########## accept command line arguments

while getopts ::s option
do
    case $option in
	s)
	    echo "symlinks enabled"
	    symlink=true;;
	?)
	builtin echo
	builtin echo "Possible command line arguments:"
	builtin echo
	builtin echo "-s      symlink dotfiles in \$HOME to those of the git repository"
	builtin echo
	exit 0;;
    esac
done

########## dotfiles that go into $HOME directory

update_file ".profile"            "$dir" "$home" $symlink
update_file ".bash_aliases"       "$dir" "$home" $symlink
update_file ".local.bashrc"       "$dir" "$home" $symlink
update_file ".touchpad_config.sh" "$dir" "$home" $symlink
update_file ".screenrc"           "$dir" "$home" $symlink
update_file ".tmux.conf"          "$dir" "$home" $symlink
update_file ".gitk"               "$dir" "$home" $symlink
update_file ".gitconfig"          "$dir" "$home" false
update_file "setup.cfg"           "$dir" "$home" $symlink

########## .bin directory

update_file ".bin" "$dir" "$home" $symlink false

########## Development tools

update_file ".config/terminator/config"      "$dir" "$home" $symlink

########## Emacs

update_file ".emacs.d/init.el" "$dir" "$home" $symlink
update_file ".emacs.d/lisp"    "$dir" "$home" $symlink false

########## Usability

update_file "40-libinput.conf"         "$dir/.peripherals" "/usr/share/X11/xorg.conf.d" $symlink
update_file "51-synaptics-quirks.conf" "$dir/.peripherals" "/usr/share/X11/xorg.conf.d" $symlink
update_file "70-synaptics.conf"        "$dir/.peripherals" "/usr/share/X11/xorg.conf.d" $symlink

########## Python

update_file ".jupyter" "$dir" "$home" $symlink false

########## closing actions

builtin echo
echo_warnings_errors
builtin echo

exit 0
