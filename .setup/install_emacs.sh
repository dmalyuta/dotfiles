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
	runcmd "wget http://ftp.gnu.org/gnu/emacs/emacs-25.1.tar.gz"
	runcmd "tar -zxvf emacs-25.1.tar.gz"
	(runcmd "cd emacs-25.1/" && configure_make_install)
	subshell_check $?
	runcmd "rm -rf emacs*"
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

echo_prefix="$echo_prefix_temp"
