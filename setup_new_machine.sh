#!/bin/bash
# ----------------------------------------------------------------------
#
# Install script for setting up my working environment on a new Linux
# computer.
#
# ----------------------------------------------------------------------

########## list of dotfiles(folders) to install

# list of possible programs to install

programs_list=(
    "home_dotfiles"
    "bin"
    "terminator"
    "emacs"
    "python"
)

# associated dotfile dependencies for each program

dependencies_home_dotfiles=(".profile" ".bash_aliases" ".local.bashrc" ".screenrc" ".tmux.conf")
dependencies_bin=(".bin")
dependencies_terminator=(".config/terminator/config")
dependencies_emacs=(".emacs.d/init.el" ".emacs.d/lisp")

# question to ask user when determining which programs to install

declare -A dotfiles_prompt
dotfiles_prompt["${programs_list[0]}"]="Do you want to install the dotfiles that go in $HOME directory [Yn]? "
dotfiles_prompt["${programs_list[1]}"]="Do you want to install the .bin directory [Yn]? "
dotfiles_prompt["${programs_list[2]}"]="Do you want to install the terminator terminal [Yn]? "
dotfiles_prompt["${programs_list[3]}"]="Do you want to install Emacs [Yn]? "
dotfiles_prompt["${programs_list[4]}"]="Do you want to install tools for Python [Yn]? "

########## global variables

dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$dir"

home="${HOME}"
dryrun=false
symlink=false
green="\e[0;32m"
cyanbold="\e[1;36m"
yellowbold="\e[1;33m"
redbold="\e[1;31m"
nostyle="\e[0m"
date=$(date +"%d%m%Y_T%H%M%S")
echo_prefix="[main setup] "
number_warnings=0
number_errors=0
os_name=$(lsb_release -si)
os_version=$(lsb_release -sr)

########## functions

source .setup/functions.sh

########## check that run as root

if [[ $EUID -ne 0 ]]; then
    echoerr "must be root (hint: run with sudo)"
    echo_warnings_errors
    exit 1
fi

########## accept command line arguments

while getopts ::ds option
do
    case $option in
	d)
	    echo "dry-run mode enabled"
	    dryrun=true;;
	s)
	    echo "symlinks enabled"
	    symlink=true;;
	?)
	builtin echo
	builtin echo "Possible command line arguments:"
	builtin echo
	builtin echo "-d      do a dry-run simulation (shows close to expected output, but does not alter any files)"
	builtin echo "-s      symlink dotfiles in \$HOME to those of the git repository"
	builtin echo
	exit 0;;
    esac
done

########## confirm that user really wants to proceed if not dry run

if [ $dryrun = false ]; then
    echowarn "not in dry run mode"
    echo "this script will change the $SUDO_USER user's dotfiles and settings in the ${home} directory"

    confirm_string="I really want to do this"
    flush_stdin
    printf_prompt "to proceed, type without the brackets exactly [${confirm_string}]"
    builtin echo
    read -p "" user_string

    if [ "$user_string" != "$confirm_string" ]; then
	echowarn "mistyped string, aborting"
	echo_warnings_errors
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
flush_stdin
for key in "${programs_list[@]}"
do
    while true
    do
	printf_prompt "${dotfiles_prompt[$key]}"
	read -r -p "" user_response
	if [[ $user_response =~ ^[yY]$ ]] || ( [[ -z $user_response ]] && [[ ${dotfiles_prompt[$key]: -5:1} == Y ]] ); then
	    # answered yes or pressed RET and yes is the default option --> add program to install list
	    install_programs_list["$key"]="yes"
	    tempvar=dependencies_$key[@]
	    install_dotfiles_list=(${install_dotfiles_list[@]} ${!tempvar})
	    break
	elif [[ $user_response =~ ^[nN]$ ]] || ( [[ -z $user_response ]] && [[ ${dotfiles_prompt[$key]: -4:1} == N ]] ); then
	    # answered no or pressed RET and no is the default option --> do not add program to install list
	    break
	fi
	# otherwise pose the question again (user entered gibberish)...
    done
done
builtin echo

########## move the dotfiles to user computer

echo "moving dotfiles to $home"

### create backups of existing dotfiles in one backup folder
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

### loop through each dotfile/folder and put it on the user computer, if requested
for foo in "${install_dotfiles_list[@]}"
do
    copy_foo "$foo" "$dir" "$home" $symlink
done

########## Emacs

install_program "emacs" .setup/install_emacs.sh

########## Installation: terminator (terminal)

install_program "terminator" .setup/install_terminator.sh

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
    runcmd "apt-get --assume-yes -f autoremove"
fi

# prompt for restart

builtin echo
echo_warnings_errors
builtin echo
flush_stdin
while true
do
    printf_prompt "Recommended to restart, do it [now/later]? "
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
