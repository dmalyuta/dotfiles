#!/bin/bash
#
# The Emacs text editor and associated tools.
#
# Author: Danylo Malyuta, 2020.

# ..:: Emacs ::..

if not_installed emacs; then
    sudo apt-get -y build-dep emacs
    sudo apt-get -y install libwebkit2gtk-4.0-dev \
	 libjansson-dev
    rm -rf /tmp/emacs/
    git clone --branch emacs-27.1 --depth 1 git://git.sv.gnu.org/emacs.git /tmp/emacs
    ( cd /tmp/emacs/ && \
	  git checkout emacs-27.1 && \
	  ./autogen.sh && \
	  ./configure --with-cairo --with-xwidgets --with-x-toolkit=gtk3 \
		      --with-modules --with-json && \
	  make && sudo make install )
fi

# ..:: Font for Emacs ::..

sudo apt-get -y install fonts-firacode

if ! ls /usr/share/fonts | grep -q FiraCode; then
    ( cd /tmp && \
	  wget -4 https://github.com/tonsky/FiraCode/files/412440/FiraCode-Regular-Symbol.zip && \
	  unzip FiraCode-Regular-Symbol.zip -d firacode && \
	  cd firacode/ && \
	  sudo -H cp * /usr/share/fonts )
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

sudo apt-get -y install curl

if not_installed pyright; then
    curl -sL https://deb.nodesource.com/setup_12.x | sudo -E bash -
    sudo apt-get -y install nodejs npm
    sudo npm install -g pyright
fi

pip install git+https://github.com/predictive-analytics-lab/data-science-types

# ..:: Language tools ::..

# >> C++ <<

if not_installed global; then
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

# ..:: Other ::..

sudo apt-get -y install fonts-powerline

# ansi-term colorization
mkdir -p ~/.terminfo
cp "$(find /usr/local/share/emacs/27.1 -name 'eterm-color')" \
   ~/.terminfo/eterm-256color
