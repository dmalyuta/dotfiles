#!/bin/bash
# ----------------------------------------------------------------------
#
# Verify and update symlinks of $HOME dotfiles to this Git
# repository's dotfiles. Use-case: use has moved the this Git
# repoistory's folder to a different folder, now symlinks of $HOME
# dotfiles are broken
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

make_symlink ".bash_profile" "$dir" "$home" $symlink
make_symlink ".bash_aliases" "$dir" "$home" $symlink
make_symlink ".local.bashrc" "$dir" "$home" $symlink

########## .bin directory

make_symlink ".bin" "$dir" "$home" $symlink

########## Emacs

make_symlink ".emacs.d/init.el" "$dir" "$home" $symlink
make_symlink ".emacs.d/lisp" "$dir" "$home" $symlink

########## Terminator

make_symlink ".config/terminator/config" "$dir" "$home" $symlink

########## closing actions

builtin echo
echo_warnings_errors
builtin echo

exit 0
