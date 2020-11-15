#!/bin/bash
#
# Installation script for the dotfiles, and to configure the Linux
# environment.
#
# Author: Danylo Malyuta, 2020.

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

# ..:: Copy dotfiles to home directory ::..

# Bash
ln -sf "$DIR"/.profile ~
ln -sf "$DIR"/.bash_aliases ~
ln -sf "$DIR"/.local.bashrc ~

if ! grep -q ".local.bashrc" ~/.bashrc; then
    { echo ""; \
      echo "# personal additions to .bashrc"; \
      echo "if [ -f ~/.local.bashrc ]; then"; \
      echo "    . ~/.local.bashrc"; \
      echo "fi"; } >> ~/.bashrc
fi


# Various executable scripts
ln -sf "$DIR"/.bin ~

# Terminal
mkdir -p ~/.config/terminator
ln -sf "$DIR"/.dircolors ~
ln -sf "$DIR"/.config/terminator/config ~/.config/terminator

# Git
cp -f "$DIR"/.gitconfig ~
ln -sf "$DIR"/.gitk ~

# Input devices
ln -sf "$DIR"/.input_config.sh ~
sudo ln -sf "$DIR"/.peripherals/40-libinput.conf /usr/share/X11/xorg.conf.d
sudo ln -sf "$DIR"/.peripherals/51-synaptics-quirks.conf /usr/share/X11/xorg.conf.d
sudo ln -sf "$DIR"/.peripherals/70-synaptics.conf /usr/share/X11/xorg.conf.d

# Python
ln -sf "$DIR"/setup.cfg ~
cp -r "$DIR"/.jupyter ~

# Emacs
mkdir -p ~/.emacs.d
ln -sf "$DIR"/.emacs.d/init.el ~/.emacs.d

# ..:: Install modules ::..

source .modules/functions.sh
source .modules/usability.sh
source .modules/dev_tools.sh
source .modules/python.sh
source .modules/julia.sh
source .modules/productivity.sh
source .modules/emacs.sh

# ..:: Cleanup ::..

sudo apt-get -yf autoremove
