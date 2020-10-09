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
    # Ubuntu 18.04,20.04 requires turning on "Sources" in ``$ software-properties-gtk``, see
    # https://askubuntu.com/questions/496549/error-you-must-put-some-source-uris-in-your-sources-list
    runcmd "apt-get --assume-yes build-dep emacs" nonull
    apt_get_install_pkg libwebkit2gtk-4.0-dev
    apt_get_install_pkg libjansson-dev
    # download source, build and install Emacs
    runcmd "rm -rf /tmp/emacs/"
    runcmd "git clone --branch emacs-27.1 --depth 1 git://git.sv.gnu.org/emacs.git /tmp/emacs"
    (runcmd "cd /tmp/emacs/" && runcmd "git checkout emacs-27.1" && runcmd "./autogen.sh" && runcmd "./configure --with-cairo --with-xwidgets --with-x-toolkit=gtk3 --with-modules --with-json" && runcmd "make" && runcmd "make install")
    # including " CFLAGS='-O3' CPPFLAGS='-O3' " in ./configure seems to make Emacs a bit unstable
fi

# Font for Emacs: Fira Code
apt_get_install_pkg fonts-firacode
(runcmd "cd /tmp" && \
     runcmd "wget -4 https://github.com/tonsky/FiraCode/files/412440/FiraCode-Regular-Symbol.zip" && \
     runcmd "unzip FiraCode-Regular-Symbol.zip -d firacode" && \
     runcmd "cd firacode/" && \
     runcmd "sudo -H cp * /usr/share/fonts")

# # Font for Emacs: Office Code Pro D
# runcmd "cd /tmp/"
# runcmd "git clone https://github.com/nathco/Office-Code-Pro"
# runcmd "cd 'Office-Code-Pro/Fonts/Office Code Pro D/TTF/'"
# runcmd "sudo -H cp * /usr/share/fonts/"

# [C++] language server protocol (clangd,ccls)

# Installation instructions:
# https://clangd.llvm.org/installation.html
apt_get_install_pkg clangd-9
runcmd "sudo update-alternatives --install /usr/bin/clangd clangd /usr/bin/clangd-9 100"

# ccls (another alternative - I've been using this one more)
if program_not_installed "ccls"; then
    apt_get_install_pkg libtinfo5
    (runcmd "cd /tmp" && \
	 runcmd "rm -rf ccls" && \
	 runcmd "git clone --depth=1 --recursive https://github.com/MaskRay/ccls" && \
	 runcmd "cd ccls" && \
	 runcmd "wget -c4 http://releases.llvm.org/8.0.0/clang+llvm-8.0.0-x86_64-linux-gnu-ubuntu-18.04.tar.xz" && \
	 runcmd "tar xf clang+llvm-8.0.0-x86_64-linux-gnu-ubuntu-18.04.tar.xz" && \
	 runcmd "cmake -H. -BRelease -DCMAKE_BUILD_TYPE=Release -DCMAKE_PREFIX_PATH=/tmp/ccls/clang+llvm-8.0.0-x86_64-linux-gnu-ubuntu-18.04" && \
	 runcmd "cmake --build Release" && \
	 runcmd "cmake --build Release --target install")
fi

# [Python] language server protocol (pyls)
# Completion and such

runcmd "pip install python-language-server[all]"
# Static type checking using mypy:
runcmd "pip install pyls-mypy"
runcmd "pip install --user future"

# [Python] language server protocol (pyright)
# Static type checking: https://github.com/microsoft/pyright
apt_get_install_pkg curl
runcmd "curl -sL https://deb.nodesource.com/setup_12.x | sudo -E bash -"
apt_get_install_pkg nodejs
apt_get_install_pkg npm
runcmd "sudo npm install -g pyright"
runcmd "pip install git+https://github.com/predictive-analytics-lab/data-science-types" # Stubs for numpy, scipy etc.

# [Python] JEDI auto-completion

runcmd "pip install virtualenv jedi"
runcmd "eval builtin echo 'export PATH=\$PATH'\":$(which virtualenv) # Python virtualenv path\" >> ${HOME}/.bashrc" nonull # add the virtualenv path to the PATH

# [Python] Flake8 linting

runcmd "pip install flake8"

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
