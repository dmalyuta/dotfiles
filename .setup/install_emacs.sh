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

	# download source, build and install Emacs 24.5
	wget_targz_install "emacs-24.5" "https://ftp.gnu.org/gnu/emacs/emacs-24.5.tar.gz"
    else
	# install emacs with apt-get install
	apt_get_install_pkg emacs
    fi
fi

# Upgrade GDB to 7.12.1 (latest stable version)
# Includes many fuxes, of which most important for Emacs is the `set max-completions`
# (see https://sourceware.org/git/gitweb.cgi?p=binutils-gdb.git;a=blobdiff;f=gdb/NEWS;h=f19577a3a6d0ea9ff1015255eafbd965580afa2d;hp=cba21b6645dd09e83943b71d42ad4c3d3c00cad4;hb=ef0b411a110cd2602cb89c3fb237baf8beb28545;hpb=e11c72c7e4879894b9711b5c0b8247c20c6050f6 and http://stackoverflow.com/questions/20933946/gdb-freezing-when-print-variables-with-tab-completion)
# Description: Sets the maximum number of candidates to be considered
# during completion. The default value is 200. This limit allows GDB
# to avoid generating large completion lists, the computation of which
# can cause the debugger to become temporarily unresponsive.
gdbVersion="$(echo $(gdb --version) | cut -d " " -f 4)"
if [[ "$gdbVersion" != "7.12.1" ]]; then
    # TODO make check only that $gdbVersion < 7.12.1 (so as not to
    # downgrade if in the future user has a more recent version
    # installed)
    wget_targz_install "gdb-7.12.1" "http://ftp.gnu.org/gnu/gdb/gdb-7.12.1.tar.gz"
fi

# [C/C++] irony-mode

apt_get_install_pkg build-essential
apt_get_install_pkg clang
apt_get_install_pkg libclang-dev

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
