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
    git clone --branch pgtk-nativecomp --depth 1 https://github.com/flatwhatson/emacs \
	/tmp/emacs
    ( cd /tmp/emacs/ && \
	  git checkout pgtk-nativecomp && \
	  export CC=/usr/bin/gcc-10 CXX=/usr/bin/gcc-10 && \
	  ./autogen.sh && \
	  # run `./configure --help > /tmp/emacs_configure_help.txt` to print
	  # out a file of configuration options
          ./configure --without-gpm --with-xwidgets --with-mailutils \
           --with-native-compilation --with-pgtk --with-json \
           CFLAGS="-O3 -mtune=native -march=native -fomit-frame-pointer" && \
	   make NATIVE_FULL_AOT=1 -j2 && \
	   sudo make install )
fi

# ..:: Language Server Protocols ::..

# >> C++ <<

sudo apt-get -y install clangd-9
sudo update-alternatives --install /usr/bin/clangd clangd /usr/bin/clangd-9 100

if not_installed ccls; then
    sudo apt-get -y install libtinfo5 \
         clang \
         libclang-10-dev \
         bear

    ( cd /tmp && \
	  rm -rf ccls && \
	  git clone --depth=1 --recursive https://github.com/MaskRay/ccls && \
	  cd ccls && \
          cmake -H. -BRelease -DCMAKE_BUILD_TYPE=Release \
                -DCMAKE_PREFIX_PATH=/usr/lib/llvm-10 \
                -DLLVM_INCLUDE_DIR=/usr/lib/llvm-10/include \
                -DLLVM_BUILD_INCLUDE_DIR=/usr/include/llvm-10/ && \
	  cmake --build Release && \
          cd Release && \
          sudo make install)
	  # cmake --build Release --target install
fi

# >> Python <<

if not_installed pyright; then
    sudo apt-get -y install curl

    curl -sL https://deb.nodesource.com/setup_12.x | sudo -E bash -
    sudo apt-get -y install nodejs
    sudo npm install -g pyright

    pip install git+https://github.com/predictive-analytics-lab/data-science-types
fi

# >> Julia <<

# Check if Julia LSP installed
echo 'using Pkg; Pkg.status()' | julia | grep LanguageServer > /dev/null 2>&1

if [ $? -ne 0 ]; then
    # Install Julia language server
    cat << EOF | julia
using Pkg
Pkg.add(PackageSpec(url="https://github.com/julia-vscode/LanguageServer.jl", rev="52dd52a"))
Pkg.add(PackageSpec(url="https://github.com/julia-vscode/StaticLint.jl", rev="3a8b7b1"))
Pkg.add("SymbolServer")
Pkg.add("CSTParser")

EOF
fi

# ..:: Language tools ::..

# >> C++ <<

if not_installed global; then
    # Dependencies
    sudo apt-get -y install libncurses5 libncurses5-dev

    wget -4 ftp://ftp.gnu.org/pub/gnu/global/global-6.5.5.tar.gz -P /tmp/
    tar -zxvf /tmp/global-6.5.5.tar.gz -C /tmp
    ( cd /tmp/global-6.5.5/ && ./configure && make && sudo make install )
fi

# >> Python <<

pip install virtualenv jedi flake8

# Add the virtualenv path to the PATH
if ! echo "$PATH" | grep -q virtualenv; then
    echo export PATH="$PATH":"$(command -v virtualenv)" >> ~/.bashrc
fi

# >> Bash <<

sudo apt-get -y install shellcheck

# ..:: Mail ::..

# >> Mu4e <<
# https://www.djcbsoftware.nl/code/mu/mu4e/Installation.html

if not_installed mu; then
    # Dependencies
    sudo apt-get -y install libgmime-3.0-dev libxapian-dev

    wget -4 https://github.com/djcb/mu/releases/download/1.4.15/mu-1.4.15.tar.xz -P /tmp/
    tar -xvf /tmp/mu-1.4.15.tar.xz -C /tmp
    ( cd /tmp/mu-1.4.15/ && ./configure && make && sudo make install )
fi

# ..:: Other ::..

# Fonts
sudo apt-get -y install fonts-powerline

# libvterm fast terminal emulation inside Emacs
sudo apt-get -y install libvterm-dev
