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

	# download source, build and install Emacs 25.1
	wget_targz_install "emacs-25.1" "https://ftp.gnu.org/gnu/emacs/emacs-25.1.tar.gz"
    else
	# install emacs with apt-get install
	apt_get_install_pkg emacs
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

# # helm-ros (an Emacs package that interfaces ROS with the helm completion facilities)
# # XXX create check so that lines below are not added to ~/.profile if they already exist there
# printf_prompt "Do you want to install helm-ros for Emacs (you should have ROS installed) [yN]? "
# read -r -p "" user_response
# if [[ $user_response =~ ^[yY]$ ]]; then
#     # answers yes --> install helm-ros
#     runcmd "eval builtin echo \"\" >> ${home}/.profile" nonull
#     runcmd "eval builtin echo \"# ROS setup\" >> ${home}/.profile" nonull

#     # source ROS's own setup.bash file
#     if [[ -f "/opt/ros/indigo/setup.bash" ]]; then
# 	runcmd "eval builtin echo \"source /opt/ros/indigo/setup.bash\" >> ${home}/.profile" nonull
#     else
# 	echowarn "Couldn't find the file /opt/ros/indigo/setup.bash."
# 	printf_prompt "Specify full file path of ROS's setup.bash file or press Enter to skip: "
# 	while true
# 	do
# 	    read -r -e -p "" user_response
# 	    if [[ -z "$user_response" ]]; then
# 		break
# 	    fi
# 	    if [[ -f "$user_response" ]]; then
# 		runcmd "eval builtin echo \"source ${user_response}\" >> ${home}/.profile" nonull
# 		break
# 	    else
# 		echowarn "Entered a bad filepath (${user_response} is not a valid file), please enter a valid complete file path to ROS's setup.bash or press the Enter key to skip"
# 	    fi
# 	done
#     fi

#     # source user's personal setup.bash file
#     if [[ -f "${home}/catkin_ws/devel/setup.bash" ]]; then
# 	runcmd "eval builtin echo \"source ${home}/catkin_ws/devel/setup.bash\" >> ${home}/.profile" nonull
#     else
# 	echowarn "Couldn't find the file /opt/ros/indigo/setup.bash."
# 	printf_prompt "Specify full file path of ROS's setup.bash file or press Enter to skip: "
# 	while true
# 	do
# 	    read -r -e -p "" user_response
# 	    if [[ -z "$user_response" ]]; then
# 		break
# 	    fi
# 	    if [[ -f "$user_response" ]]; then
# 		runcmd "eval builtin echo \"source ${user_response}\" >> ${home}/.profile" nonull
# 		break
# 	    else
# 		echowarn "Entered a bad filepath (${user_response} is not a valid file), please enter a valid complete file path to ROS's setup.bash or press the Enter key to skip"
# 	    fi
# 	done
#     fi
# fi

# echo_prefix="$echo_prefix_temp"
