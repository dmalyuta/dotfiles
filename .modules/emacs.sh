#!/bin/bash
#
# The Emacs text editor and associated tools.
#
# Author: Danylo Malyuta, 2020.

# ..:: Emacs ::..

if not_installed emacs; then
    # Dependencies
    sudo apt-get -y build-dep emacs
    sudo apt-get -y install libwebkit2gtk-4.0-dev \
	 libjansson-dev \
	 autoconf \
	 texinfo \
	 libncurses-dev

    rm -rf /tmp/emacs/
    git clone --branch emacs-27.1.90 --depth 1 git://git.sv.gnu.org/emacs.git \
	/tmp/emacs
    ( cd /tmp/emacs/ && \
	  git checkout emacs-27.1.90 && \
	  ./autogen.sh && \
	  # run `./configure --help > /tmp/emacs_configure_help.txt` to print
	  # out a file of configuration options
	  ./configure --with-x-toolkit=lucid --with-json \
		      CFLAGS='-O3 -march=native -pipe' \
		      ' -falign-functions=64 -fomit-frame-pointer -ftracer' \
		      ' -funit-at-a-time -fweb -fforce-addr -fpeel-loops' \
		      ' -funswitch-loops -frename-registers -mfpmath=sse' \
		      ' -ffast-math -fno-finite-math-only -fstack-check' && \
	      make && sudo make install )
fi

# ..:: Language Server Protocols ::..

# >> C++ <<

sudo apt-get -y install clangd-9
sudo update-alternatives --install /usr/bin/clangd clangd /usr/bin/clangd-9 100

if not_installed ccls; then
    sudo apt-get -y install libtinfo5
    ( cd /tmp && \
	  rm -rf ccls && \
	  git clone --depth=1 --recursive https://github.com/MaskRay/ccls && \
	  cd ccls && \
	  wget -c4 http://releases.llvm.org/8.0.0/clang+llvm-8.0.0-x86_64-linux-gnu-ubuntu-18.04.tar.xz && \
	  tar xf clang+llvm-8.0.0-x86_64-linux-gnu-ubuntu-18.04.tar.xz && \
	  cmake -H. -BRelease -DCMAKE_BUILD_TYPE=Release \
		-DCMAKE_PREFIX_PATH=/tmp/ccls/clang+llvm-8.0.0-x86_64-linux-gnu-ubuntu-18.04 && \
	  cmake --build Release && \
	  cmake --build Release --target install )
fi

# >> Python <<

if not_installed pyright; then
    sudo apt-get -y install curl

    curl -sL https://deb.nodesource.com/setup_12.x | sudo -E bash -
    sudo apt-get -y install nodejs npm
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
Pkg.add(PackageSpec(url="https://github.com/julia-vscode/LanguageServer.jl"))
Pkg.add(PackageSpec(url="https://github.com/julia-vscode/StaticLint.jl"))
Pkg.add(PackageSpec(url="https://github.com/julia-vscode/SymbolServer.jl"))
Pkg.add([Artifacts","ZipFile","BinaryProvider","Requires"])
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

    wget -4 https://github.com/djcb/mu/releases/download/1.4.13/mu-1.4.13.tar.xz -P /tmp/
    tar -zxvf /tmp/mu-1.4.13.tar.xz -C /tmp
    ( cd /tmp/mu-1.4.13/ && ./configure && make && sudo make install )
fi

# ..:: Other ::..

sudo apt-get -y install fonts-powerline

# ansi-term colorization
mkdir -p ~/.terminfo
cp "$(find /usr/local/share/emacs/27.1 -name 'eterm-color')" \
   ~/.terminfo/eterm-256color
