#!/bin/bash
#
# The Emacs text editor and associated tools.
#
# Author: Danylo Malyuta, 2020.

# ..:: Emacs ::..

# Source: https://blog.cesarolea.com/posts/emacs-native-compile/index.html

if not_installed emacs; then
    # Dependencies
    sudo apt-get -y install \
        autoconf \
        texinfo \
        libncurses-dev \
        libxpm-dev \
        libgif-dev \
        libjpeg-dev \
        libpng-dev \
        libtiff-dev \
        librsvg2-dev \
        libmagick++-dev \
        libx11-dev \
        libncurses5-dev \
        automake \
        autoconf \
        texinfo \
        libgtk2.0-dev \
        gcc-14 \
        g++-14 \
        libgccjit0 \
        libgccjit-14-dev \
        libjansson4 \
        libjansson-dev \
        libgnutls28-dev \
        libgpm-dev

    # Treesitter.
    # https://www.masteringemacs.org/article/how-to-get-started-tree-sitter
    rm -rf /tmp/treesitter/
    git clone https://github.com/tree-sitter/tree-sitter.git /tmp/treesitter
    (cd /tmp/treesitter && make && sudo make install)
    export LD_LIBRARY_PATH=/usr/local/lib/
    sudo ldconfig

    rm -rf /tmp/emacs/
    git clone --depth 1 --branch emacs-30.1 git://git.savannah.gnu.org/emacs.git /tmp/emacs
    (cd /tmp/emacs/ &&
        export CC=/usr/bin/gcc CXX=/usr/bin/gcc &&
        export CFLAGS="-O3 -march=native -mtune=native -flto=auto -fuse-linker-plugin -pipe -fomit-frame-pointer" &&
        export CXXFLAGS="$CFLAGS" &&
        export LDFLAGS="-Wl,-O3 -Wl,--as-needed -flto=auto" &&
        ./autogen.sh &&
        # run `./configure --help > /tmp/emacs_configure_help.txt` to print out a file of
        # configuration options
        ./configure \
            --with-native-compilation \
            --enable-link-time-optimization \
            --with-tree-sitter \
            --disable-gc-mark-trace \
            --with-json \
            --with-x-toolkit=gtk3 \
            --with-xinput2 \
            --with-imagemagick \
            --with-rsvg &&
        make NATIVE_FULL_AOT=1 -j2 &&
        sudo make install)
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

# >> C++ <<
sudo apt-get -y install clang-format

# >> Language servers <<
if not_installed cargo; then
    # Install Rust's package manager - Cargo.
    # https://doc.rust-lang.org/cargo/getting-started/installation.html
    curl https://sh.rustup.rs -sSf | sh
fi
# https://github.com/blahgeek/emacs-lsp-booster
cargo install emacs-lsp-booster

# ..:: Mathjax preview for Org mode ::..

if not_installed math-preview; then
    # https://gitlab.com/matsievskiysv/math-preview
    npm install -g git+https://gitlab.com/matsievskiysv/math-preview
    (cd /usr/local/bin &&
        sudo ln -sf ~/.nvm/versions/node/v22.0.0/bin/math-preview .)
fi

# ..:: Other ::..

# Fonts
sudo apt-get -y install fonts-powerline

# libvterm fast terminal emulation inside Emacs
sudo apt-get -y install libvterm-dev
if [[ ! -d ~/.emacs.d/libvterm ]]; then
    git clone https://github.com/akermu/emacs-libvterm.git \
        ~/.emacs.d/libvterm
    (cd ~/.emacs.d/libvterm &&
        mkdir -p build &&
        cd build &&
        cmake .. &&
        make)
fi
