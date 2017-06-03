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
    # install from source
    # install dependencies
    runcmd "apt-get --assume-yes build-dep emacs24" nonull
    # download source, build and install Emacs 25.2
    runcmd "wget https://ftp.gnu.org/gnu/emacs/emacs-25.2.tar.gz -O /tmp/emacs.tar.gz"
    runcmd "mkdir -p /tmp/emacs"
    runcmd "tar zxf /tmp/emacs.tar.gz --strip 1 -C /tmp/emacs"
    (runcmd "cd /tmp/emacs/" && runcmd "./configure --with-x-toolkit=lucid" && runcmd "make" && runcmd "make install")
fi

# [C/C++] YCMD completion engine
# Find a default .ycm_extra_conf.py file (for setting up compilation flags to get completion working)
# in .emacs.d/.ycm_extra_conf.py

apt_get_install_pkg build-essential
apt_get_install_pkg clang
apt_get_install_pkg libclang-dev
apt_get_install_pkg cmake
apt_get_install_pkg python-dev

if [ ! -d "${home}/.emacs.d/ycmd" ]; then
    # Install YCMD
    runcmd "git clone https://github.com/Valloric/ycmd ${home}/.emacs.d/ycmd"
    (runcmd "cd ${home}/.emacs.d/ycmd" && runcmd "git submodule update --init --recursive" && runcmd "./build.py --clang-completer")
    runcmd "eval chown -R ${SUDO_USER:-$USER}:${SUDO_USER:-$USER} ${home}/.emacs.d/ycmd"
fi

# [Python] JEDI auto-completion

runcmd "sudo -H pip install virtualenv jedi"
runcmd "eval builtin echo 'export PATH=\$PATH'\":$(which virtualenv) # Python virtualenv path\" >> ${HOME}/.bashrc" nonull # add the virtualenv path to the PATH

# [Python] Flake8 linting

runcmd "sudo -H python -m pip install flake8"

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
