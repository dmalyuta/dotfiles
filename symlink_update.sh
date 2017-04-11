#!/bin/bash
# ----------------------------------------------------------------------
#
# Verify and update symlinks of $HOME dotfiles to this Git
# repository's dotfiles. Use-case: use has moved the this Git
# repository's folder to a different folder, now symlinks of $HOME
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

if [ -e "${home}/.profile" ]; then
    make_symlink ".profile" "$dir" "$home" $symlink
fi
if [ -e "${home}/.bash_aliases" ]; then
    make_symlink ".bash_aliases" "$dir" "$home" $symlink
fi
if [ -e "${home}/.local.bashrc" ]; then
    make_symlink ".local.bashrc" "$dir" "$home" $symlink
fi
if [ -e "${home}/.screenrc" ]; then
    make_symlink ".screenrc" "$dir" "$home" $symlink
fi
if [ -e "${home}/.tmux.conf" ]; then
    make_symlink ".tmux.conf" "$dir" "$home" $symlink
fi

########## .bin directory

if [ -e "${home}/.bin" ]; then
    make_symlink ".bin" "$dir" "$home" $symlink
fi

########## Development tools

if [ -e "${home}/.config/terminator/config" ]; then
    make_symlink ".config/terminator/config" "$dir" "$home" $symlink
fi
if [ -e "${home}/.icons/matlab_icon.png" ]; then
    make_symlink ".icons/matlab_icon.png" "$dir" "$home" $symlink
fi
if [ -e "${home}/.icons/arbre_analyste_icon.png" ]; then
    make_symlink ".icons/arbre_analyste_icon.png" "$dir" "$home" $symlink
fi

########## Emacs

if [ -e "${home}/.emacs.d/init.el" ]; then
    make_symlink ".emacs.d/init.el" "$dir" "$home" $symlink
fi
if [ -e "${home}/.emacs.d/lisp" ]; then
    make_symlink ".emacs.d/lisp" "$dir" "$home" $symlink
fi

########## Python

if [ -e "${home}/.jupyter/jupyter_notebook_config.py" ]; then
    make_symlink ".jupyter/jupyter_notebook_config.py" "$dir" "$home" $symlink
fi

########## closing actions

builtin echo
echo_warnings_errors
builtin echo

exit 0
