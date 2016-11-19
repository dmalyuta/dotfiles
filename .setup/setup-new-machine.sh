#!/bin/bash
# ----------------------------------------------------------------------
#
# Install script for setting up my working environment on a new Linux
# computer. Tested with: Linux Mint 18 MATE 64-bit. Works also for
# existing installations.
#
# ----------------------------------------------------------------------

########## go to directory where setup.sh actually is
# necessary in case script is called from another directory

realdir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$realdir"

########## list of dotfiles(folders) to install

# list of possible programs to install

programs_list=(
    "bin"
    "git"
    "emacs"
    "i3"
    "terminator"
    "powerline"
    "chrome"
    "foxit"
)

# associated dotfile dependencies for each program

dependencies_bin=(".bin")
dependencies_emacs=(".emacs.d")
dependencies_i3=(
    ".fonts"
    ".i3"
    ".Xresources"
    ".gtkrc-2.0"
    ".config/gtk-3.0/settings.ini"
    ".dmrc"
)
dependencies_terminator=(".config/terminator/config")

# question to ask user when determining which programs to install

declare -A dotfiles_prompt
dotfiles_prompt["${programs_list[0]}"]="Do you want to install the .bin directory [Yn]? "
dotfiles_prompt["${programs_list[1]}"]="Do you want to install Git [Yn]? "
dotfiles_prompt["${programs_list[2]}"]="Do you want to install Emacs [Yn]? "
dotfiles_prompt["${programs_list[3]}"]="Do you want to install the i3 window manager and associated themes [Yn]? "
dotfiles_prompt["${programs_list[4]}"]="Do you want to install the terminator terminal emulator [Yn]? "
dotfiles_prompt["${programs_list[5]}"]="Do you want to install powerline [Yn]? "
dotfiles_prompt["${programs_list[6]}"]="Do you want to install Google Chrome [Yn]? "
dotfiles_prompt["${programs_list[7]}"]="Do you want to install Foxit PDF Reader [Yn]? "

########## global variables

dir="$(dirname "$realdir")"
home="${HOME}"
dryrun=false
green="\e[0;32m"
cyanbold="\e[1;36m"
yellowbold="\e[1;33m"
redbold="\e[1;31m"
nostyle="\e[0m"
date=$(date +"%d%m%Y_T%H%M%S")
echo_prefix="[main setup] "

########## functions

source functions.sh

########## check that run as root

if [[ $EUID -ne 0 ]]; then
    echoerr "must be root (hint: run with sudo)"
    exit 1
fi

########## accept command line arguments

while getopts ::d option
do
    case $option in
	d) echo "dry-run mode enabled"
	   dryrun=true;;
    esac
done

########## confirm that user really wants to proceed if not dry run

if [ $dryrun = false ]; then
    echowarn "not in dry run mode"
    echo "this script will change the $SUDO_USER user's dotfiles and settings in the ${home} directory"

    confirm_string="I really want to do this"
    printf_prompt "to proceed, type without the brackets exactly [${confirm_string}]"
    builtin echo
    read -p "" user_string

    if [ "$user_string" != "$confirm_string" ]; then
	echowarn "mistyped string, aborting"
	exit 1
    fi
fi

########## prompt user for which dotfiles/features he/she wants installed

builtin echo
echo "Answer the following questions to determine which programs and dotfiles to install"
builtin echo
echo "Type either y or Y for \"yes\" and anything else for \"no\". The default answer is capitalized"
echo "in the brackets following the question. Pressing RET selects the default answer."
builtin echo

declare -A install_programs_list
declare -a install_dotfiles_list
for key in "${programs_list[@]}"
do
    printf_prompt "${dotfiles_prompt[$key]}"
    read -r -p "" user_response
    if [[ $user_response =~ ^[yY]$ ]] || [[ -z $user_response ]] && [[ ${dotfiles_prompt[$key]: -5:1} == Y ]]; then
	# answered yes or pressed RET and yes is the default option --> add program to install list
	install_programs_list["$key"]="yes"

	tempvar=dependencies_$key[@]
	install_dotfiles_list=(${install_dotfiles_list[@]} ${!tempvar})
    fi
done
builtin echo

########## make backups of existing dotfiles

echo "moving dotfiles to $home"
if [ "$dir" != "$home" ]; then
    # create backup folder
    backup_folder="${home}/dotfile_backup_$date"
    temp="$backup_folder"
    counter=0
    while [ -d "$backup_folder" ]
    do
	((counter++))
	backup_folder="${temp}_$counter"
    done
    runcmd "sudo -u $SUDO_USER mkdir $backup_folder"
    echo "any existing dotfiles in $home will be moved to $backup_folder"
    # loop through each dotfile/folder and back them up
    for foo in "${install_dotfiles_list[@]}"
    do
	copy_foo "$foo" "$dir" "$home"
    done
else
    echowarn "already in $home so will not move/backup anything"
fi

########## git

apt_get_install_pkg git

########## Emacs

install_program "emacs" install_emacs.sh

########## i3 window manager

install_program "i3" install_i3.sh

########## terminator (terminal)

install_program "terminator" install_terminator.sh

########## powerline for bash

install_program "powerline" install_powerline.sh 

########## Google Chrome

install_program "chrome" install_chrome.sh

########## Foxit PDF Reader

install_program "foxit" install_foxit.sh

########## closing actions

# delete backup folder if it's empty (nothing was backed up)

if [ -d "$backup_folder" ]; then
    if ! find "$backup_folder" -mindepth 1 -print -quit | grep -q .; then
	echo "$backup_folder is empty (no existing dotfiles found in ${home}), deleting it"
	runcmd "rmdir $backup_folder"
    fi
fi

# remove not needed packages

if [ ${#install_programs_list[@]} -ne 0 ]; then
    runcmd "apt-get --assume-yes autoremove"
fi

# prompt for restart

builtin echo
while true
do
    printf_prompt "finished successfully, recommended to restart [now/later]? "
    read -r -p "" user_response
    if [[ "$user_response" =~ ^[nN][oO][wW]$ ]]; then
	builtin echo
	runcmd "shutdown -r now"
	break
    elif [[ "$user_response" =~ ^[lL][aA][tT][eE][rR]$ ]]; then
	break
    fi
    echowarn "please enter either 'now' or 'later' (without the quotes)"
done

# exit program successfully

exit 0
