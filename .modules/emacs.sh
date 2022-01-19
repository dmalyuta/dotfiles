#!/bin/bash
#
# The Emacs text editor and associated tools.
#
# Author: Danylo Malyuta, 2020.

# ..:: Emacs ::..

# Source: https://blog.cesarolea.com/posts/emacs-native-compile/index.html

if not_installed emacs; then
    # Dependencies
    sudo apt-get -y build-dep emacs
    sudo apt-get -y install libwebkit2gtk-4.0-dev \
	 autoconf \
	 texinfo \
	 libncurses-dev

    # Get libgcc and other libs for native compilation
    sudo add-apt-repository -y ppa:ubuntu-toolchain-r/ppa
    sudo apt-get update
    sudo apt-get -y install libxpm-dev \
        libgif-dev \
        libjpeg-dev \
        libpng-dev \
        libtiff-dev \
        libx11-dev \
        libncurses5-dev \
        automake \
        autoconf \
        texinfo \
        libgtk2.0-dev \
        gcc-10 \
        g++-10 \
        libgccjit0 \
        libgccjit-10-dev \
        libjansson4 \
        libjansson-dev

    # Get fast JSON
    sudo apt-get -y install libjansson4 libjansson-dev

    rm -rf /tmp/emacs/
    git clone --branch master --depth 1 git://git.savannah.gnu.org/emacs.git \
	/tmp/emacs
    ( cd /tmp/emacs/ && \
	  git checkout master && \
	  export CC=/usr/bin/gcc-10 CXX=/usr/bin/gcc-10 && \
	  ./autogen.sh && \
	  # run `./configure --help > /tmp/emacs_configure_help.txt` to print
	  # out a file of configuration options
          ./configure --without-gpm --with-mailutils --with-native-compilation \
                      --with-json --with-pgtk --with-xwidgets --with-xinput2 \
                      CFLAGS="-O3 -mtune=native -march=native -fomit-frame-pointer" && \
	  make NATIVE_FULL_AOT=1 -j2 && \
	  sudo make install )
fi

# ..:: Configuration ::..

mkdir -p ~/.emacs.d
mkdir -p ~/.emacs.d/lisp
ln -sf "$DIR"/.emacs.d/init.el ~/.emacs.d
ln -sf "$DIR"/.emacs.d/early-init.el ~/.emacs.d
ln -sf "$DIR"/.emacs.d/lisp/danylo-custom-variables.el ~/.emacs.d/lisp
ln -sf "$DIR"/.emacs.d/lisp/danylo-common-font-lock.el ~/.emacs.d/lisp
ln -sf "$DIR"/.emacs.d/lisp/danylo-prog-font-lock.el ~/.emacs.d/lisp
ln -sf "$DIR"/.emacs.d/lisp/danylo-text-font-lock.el ~/.emacs.d/lisp
ln -sf "$DIR"/.emacs.d/lisp/snippets ~/.emacs.d/lisp/

# ..:: Language tools ::..

# >> Bash <<
sudo apt-get -y install shellcheck

# ..:: Other ::..

# Fonts
sudo apt-get -y install fonts-powerline

# libvterm fast terminal emulation inside Emacs
sudo apt-get -y install libvterm-dev
if [[ ! -d ~/.emacs.d/libvterm ]]; then
    git clone https://github.com/akermu/emacs-libvterm.git \
        ~/.emacs.d/libvterm
    ( cd ~/.emacs.d/libvterm && \
          mkdir -p build && \
          cd build && \
          cmake .. && \
          make )
fi
