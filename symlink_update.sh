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
green="\e[0;32m"
cyanbold="\e[1;36m"
yellowbold="\e[1;33m"
redbold="\e[1;31m"
nostyle="\e[0m"
echo_prefix="[default] "
number_warnings=0
number_errors=0

########## basic functions

source .setup/functions.sh

########## specialized functions

make_symlink()
{
    local name="$1"
    
    echo_prefix="[${name}] "
    git_version="${dir}/$name"
    original_version="${home}/$name"
    if [ ! -e "$git_version" ]; then
	echoerr "couldn't find $git_version"
	builtin echo
	echo_warnings_errors
	builtin echo
	exit 1
    fi
    home_version_parent_dir="$(dirname "$git_version")"
    makefolder "$home_version_parent_dir"
    runcmd "eval /bin/cp -asrf \"$git_version\" \"${home_version_parent_dir}\""
}

########## .bin directory

make_symlink ".bin"

########## Emacs

make_symlink ".emacs.d/init.el"
make_symlink ".emacs.d/lisp"

########## Terminator

make_symlink ".config/terminator/config"

########## closing actions

builtin echo
echo_warnings_errors
builtin echo

exit 0
