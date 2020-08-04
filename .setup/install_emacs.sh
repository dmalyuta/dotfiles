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
    # Ubuntu 18.04 requires turning on "Sources" in ``$ software-properties-gtk``, see
    # https://askubuntu.com/questions/496549/error-you-must-put-some-source-uris-in-your-sources-list
    runcmd "apt-get --assume-yes build-dep emacs25" nonull
    # download source, build and install Emacs
    runcmd "git clone --branch emacs-27.1-rc1 --depth 1 git://git.sv.gnu.org/emacs.git /tmp/emacs"
    (runcmd "cd /tmp/emacs/" && runcmd "git checkout emacs-27.1-rc1" && runcmd "./autogen.sh" && runcmd "./configure --with-x-toolkit=lucid" && runcmd "make" && runcmd "make install")
fi

# # Font for Emacs: Office Code Pro D
# runcmd "cd /tmp/"
# runcmd "git clone https://github.com/nathco/Office-Code-Pro"
# runcmd "cd 'Office-Code-Pro/Fonts/Office Code Pro D/TTF/'"
# runcmd "sudo -H cp * /usr/share/fonts/"

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
