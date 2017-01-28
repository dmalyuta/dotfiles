#!/bin/bash
# ----------------------------------------------------------------------
#
# Emacs installation.
#
# An extensible, customizable, free/libre text editor - and more. At
# its core is an interpreter for Emacs Lisp, a dialect of the Lisp
# programming language with extensions to support text editing.
#
# To learn more about Emacs, visit:
# https://www.gnu.org/software/emacs/
#
# ----------------------------------------------------------------------

echo_prefix_temp="$echo_prefix"
echo_prefix="[emacs setup] "

# Emacs itself

if program_not_installed "emacs"; then
    if [[ "$os_name" == "Ubuntu" ]] && [[ ${os_version:0:2} -lt 15 ]]; then
	# install from source

	# install dependencies
	runcmd "apt-get --assume-yes build-dep emacs24" nonull

	# download source, build and install Emacs 24.5.1
	wget_targz_install "emacs-24.5" "http://ftp.gnu.org/gnu/emacs/emacs-24.5.tar.gz"
    else
	# install emacs with apt-get install
	apt_get_install_pkg emacs

	# add alias to bash to start Emacs as fast as vim!
	runcmd "eval builtin echo \"alias em='emacs'\" >> ${home}/.bashrc" nonull
    fi
fi

# irony-mode

apt_get_install_pkg build-essential
apt_get_install_pkg clang
apt_get_install_pkg libclang-dev

# flycheck in shell-script-mode

apt_get_install_pkg shellcheck

# helm-gtags GNU GLOBAL
if program_not_installed "global"; then
    # GNU GLOBAL not installed --> install it

    # install dependencies    
    apt_get_install_pkg libncurses5
    apt_get_install_pkg libncurses5-dev

    # install GNU GLOBAL
    wget_targz_install "global-6.5.5" "ftp://ftp.gnu.org/pub/gnu/global/global-6.5.5.tar.gz"
fi

# rosemacs (ROS tools for Emacs)
# printf_prompt "Do you want to install ROS tools for Emacs (must have ROS installed) [yN]? "
# read -r -p "" user_response
# if [[ $user_response =~ ^[yY]$ ]]; then
#     # answers yes --> install rosemacs (ROS tools for Emacs)
#     if [[ ! -d "/opt/ros/indigo" ]] ; then
# 	# ROS indigo seems to not be installed
# 	echoerr "Setup configured only for ROS indigo installed in /opt/ros/indigo directory. This seems to not be the case for you... please do the installation manually by following the steps on http://wiki.ros.org/rosemacs"
#     else
# 	runcmd "apt-get download ros-indigo-rosemacs"
# 	runcmd "dpkg --force-all -i ros*.deb"
# 	runcmd "rm -rf ros*.deb"
#     fi
# fi

echo_prefix="$echo_prefix_temp"
