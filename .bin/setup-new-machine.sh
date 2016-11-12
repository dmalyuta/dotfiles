#!/bin/bash
# ----------------------------------------------------------------------
#
# Install script for settings up my working environment on a new Linux
# computer. Tested with: Linux Mint 18 MATE 64-bit.
#
# ----------------------------------------------------------------------

########## go to directory where setup.sh actually is
# necessary in case script is called from another directory

realdir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$realdir"

########## global variables

dir="$(dirname $(pwd))"
home="${HOME}"
dotfiles_list=(".bin" ".fonts" ".emacs.d" ".i3" ".Xresources" ".gtkrc-2.0" ".config/gtk-3.0/settings.ini" ".dmrc")
dryrun=false
yellowbold="\033[1;33m"
redbold="\033[1;31m"
nostyle="\033[0m"

########## functions

echoerr()
{ # output to STDERR (standard error stream)
    echo
    echo -e "${redbold}error${nostyle}: $@" 1>&2
    echo
}

echowarn()
{ # output a warning
    echo
    echo -e "${yellowbold}warning${nostyle}: $@"
    echo
}

runcmd()
{ # run a command in active mode, just print it in dry run mode
    if [ "$2" ]; then
	# disable 1>/dev/null which prevents from writing to file
	local cmd="$1"
    else
	local cmd="$1 1>/dev/null"
    fi
    echo "$1"
    if [ $dryrun = false ]; then
	eval $cmd
	local exit_status=$?
	if [ $exit_status -ne 0 ]; then
	    echoerr "the last command failed (exit status ${exit_status}), please see why and rerun the script"
	    exit 1
	fi
    fi
}

apt_get_install_pkg()
{ # install package with apt-get
    local pkg="$1"
    runcmd "apt-get --assume-yes install $pkg"
}

copy_foo()
{
    local foo="$1"
    local dir="$2"
    local dest="$3"
    
    local git_foo="${dir}/$foo" # new folder from repository
    local home_foo="${dest}/$foo" # existing folder in ${HOME}
    if [ -e "$git_foo" ]; then
	# move existing foo to backup folder
	if [ -e "$home_foo" ]; then
	    runcmd "mv $home_foo $backup_folder"
	fi
	# copy git repo folder to home, preserving permissions
	makefolder "$dest"
	runcmd "cp -rp $git_foo $home_foo"
    else
	echowarn "couldn't find $git_foo"
    fi
}

makefolder()
{
    local folder="$1"
    if [ ! -d "$folder" ]; then
	runcmd "mkdir -p $folder"
    fi
}

move_foo()
{
    local foo="$1"
    local source="$2"
    local destination="$3"

    copy_foo "$foo" "$source" "$destination"
    runcmd "rm -rf ${source}/${foo}"
}

configure_make_install()
{
    runcmd "./configure"
    runcmd "make"
    runcmd "make install"
}

wget_targz_install()
{
    local name="$1"
    local url="$2"
        
    runcmd "wget $url"
    runcmd "tar -zxvf ${name}.tar.gz"
    runcmd "rm -rf ${name}.tar.gz"
    (runcmd "cd ${name}/" && configure_make_install)
    runcmd "rm -rf ${name}/"
}

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
    echo "to proceed, type without the brackets exactly [${confirm_string}]"
    read -p "" user_string

    if [ "$user_string" != "$confirm_string" ]; then
	echowarn "mistyped string, aborting"
	exit 1
    fi
fi

########## make backups of existing dotfiles

echo "moving dotfiles to $home"
if [ "$dir" != "$home" ]; then
    # create backup folder
    backup_folder="${home}/dotfile_backup"
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
    for foo in "${dotfiles_list[@]}"
    do
	copy_foo "$foo" "$dir" "$home"
    done
else
    echowarn "already in $home so will not move/backup anything"
fi

########## git

apt_get_install_pkg git

########## Emacs

# Emacs itself

apt_get_install_pkg emacs

# irony-mode

apt_get_install_pkg build-essential
apt_get_install_pkg clang
apt_get_install_pkg libclang-dev

# flycheck in shell-script-mode

apt_get_install_pkg shellcheck

# helm-gtags GNU GLOBAL
if ! global --version >/dev/null 2>&1; then
    # GNU GLOBAL not installed --> install it

    # install dependencies
    
    apt_get_install_pkg libncurses5
    apt_get_install_pkg libncurses5-dev

    # install GNU GLOBAL
    wget_targz_install "global-6.5.5" "ftp://ftp.gnu.org/pub/gnu/global/global-6.5.5.tar.gz"
fi

########## i3 window manager

# i3 itself

apt_get_install_pkg i3
apt_get_install_pkg i3status
apt_get_install_pkg i3lock
apt_get_install_pkg i3blocks
apt_get_install_pkg feh
apt_get_install_pkg sysstat
apt_get_install_pkg acpi
apt_get_install_pkg compton

# numix theme and icons

runcmd "add-apt-repository -y ppa:numix/ppa"
runcmd "apt-get update"
apt_get_install_pkg numix-gtk-theme
apt_get_install_pkg numix-icon-theme

# arc-theme

if ! dpkg -l "arc-theme" >/dev/null 2>&1; then
    # arc-theme not installed --> install it
    # following instructions: http://software.opensuse.org/download.html?project=home%3AHorst3180&package=arc-theme
    
    runcmd "wget http://download.opensuse.org/repositories/home:Horst3180/xUbuntu_16.04/Release.key"
    runcmd "apt-key add - < Release.key"
    runcmd "rm Release.key"
    runcmd "apt-get update"
    runcmd "sh -c \"echo 'deb http://download.opensuse.org/repositories/home:/Horst3180/xUbuntu_16.04/ /' > /etc/apt/sources.list.d/arc-theme.list\""
    runcmd "apt-get update"
    apt_get_install_pkg arc-theme
fi

# playerctl

if ! playerctl --version >/dev/null 2>&1; then
    # playerctl not installed --> install it
    runcmd "wget https://github.com/acrisci/playerctl/releases/download/v0.5.0/playerctl-0.5.0_amd64.deb"
    runcmd "dpkg -i playerctl-0.5.0_amd64.deb"
    runcmd "rm -rf playerctl-0.5.0_amd64.deb"
fi

# rofi

if ! rofi -version >/dev/null 2>&1; then
    # rofi not installed --> install it

    # install dependencies
    apt_get_install_pkg libpango-1.0-0
    apt_get_install_pkg libpangocairo-1.0-0
    apt_get_install_pkg libcairo2
    apt_get_install_pkg xcb
    apt_get_install_pkg libxcb-util-dev
    apt_get_install_pkg libxcb-ewmh-dev
    apt_get_install_pkg libxcb-xinerama0
    apt_get_install_pkg libxcb-xinerama0-dev
    apt_get_install_pkg libxcb-icccm4-dev
    apt_get_install_pkg libxcb-xkb-dev
    apt_get_install_pkg libstartup-notification0
    apt_get_install_pkg libstartup-notification0-dev
    apt_get_install_pkg libxkbcommon-x11-dev
    apt_get_install_pkg libxkbcommon0
    apt_get_install_pkg libxkbcommon-dev
    apt_get_install_pkg libglib2.0-dev
    apt_get_install_pkg libperl-dev
    apt_get_install_pkg libgtk2.0-dev
    apt_get_install_pkg xutils
    apt_get_install_pkg xutils-dev

    # install xcb-util-xrm dependency
    # TODO: somehow check if installed already
    
    wget_targz_install "xcb-util-xrm-1.0" "https://github.com/Airblader/xcb-util-xrm/releases/download/v1.0/xcb-util-xrm-1.0.tar.gz"
    
    # install rofi

    wget_targz_install "rofi-1.2.0" "https://github.com/DaveDavenport/rofi/releases/download/1.2.0/rofi-1.2.0.tar.gz"
    
    # make sure that shared libraries in /usr/local/lib/ are seen
    
    if [ ! -f  "/etc/ld.so.conf.d/usr-local.conf" ]; then
	runcmd "touch /etc/ld.so.conf.d/usr-local.conf"
    fi
    runcmd "echo \"/usr/local/lib\" > /etc/ld.so.conf.d/usr-local.conf"
    runcmd "ldconfig"
fi

########## powerline for bash

# install powerline
# following instructions: https://powerline.readthedocs.io/en/latest/installation.html#pip-installation

apt_get_install_pkg python3
apt_get_install_pkg python-pip
apt_get_install_pkg fontconfig
runcmd "sudo -H pip install --upgrade pip"
runcmd "sudo -H pip install setuptools"
runcmd "sudo -H pip install powerline-status"

# install powerline fonts
# following instructions: https://powerline.readthedocs.io/en/latest/installation/linux.html

runcmd "wget https://github.com/powerline/powerline/raw/develop/font/PowerlineSymbols.otf"
runcmd "wget https://github.com/powerline/powerline/raw/develop/font/10-powerline-symbols.conf"

font_dir="${home}/.fonts"
makefolder "$font_dir"
move_foo "PowerlineSymbols.otf" "$realdir" "$font_dir"
runcmd "fc-cache -vf ${font_dir}"

fontconfig_dir="${home}/.config/fontconfig/conf.d"
makefolder "$fontconfig_dir"
move_foo "10-powerline-symbols.conf" "$realdir" "$fontconfig_dir"

# enable powerline for bash
# following instructions: https://powerline.readthedocs.io/en/latest/usage/shell-prompts.html#bash-prompt
repository_root=$(sudo -H pip show powerline-status | grep Location | cut -d " " -f 2)
msg=". ${repository_root}/powerline/bindings/bash/powerline.sh"
if ! $(cat "${home}/.bashrc" | grep "export TERMS="); then
    # line not already in ~/.bashrc, so append it
    runcmd "echo \"${msg}\" >> ${home}/.bashrc" nonull
fi

# configure powerline
# following instructions: https://github.com/adidenko/powerline
runcmd "git clone https://github.com/adidenko/powerline"
move_foo "powerline" "$realdir" "${home}/.config"

########## Google Chrome

# remove Firefox

runcmd "apt-get --assume-yes purge firefox"
runcmd "apt-get --assume-yes autoremove"

# install Chrome's dependencies

apt_get_install_pkg libxss1
apt_get_install_pkg apt_get_install_pkg
apt_get_install_pkg apt_get_install_pkg

# Chrome itself

if ! google-chrome --version >/dev/null 2>&1; then
    runcmd "wget https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb"
    runcmd "dpkg -i google-chrome*.deb"
    runcmd "rm -rf google-chrome*.deb"
fi

########## closing actions

if [ -d "$backup_folder" ]; then
    if ! find "$backup_folder" -mindepth 1 -print -quit | grep -q .; then
	echo "$backup_folder is empty (no existing dotfiles found in ${home}), deleting it"
	runcmd "rmdir $backup_folder"
    fi
fi

echo "finished successfully"

exit 0
