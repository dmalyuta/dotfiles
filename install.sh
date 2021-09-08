#!/bin/bash
#
# Installation script for the dotfiles, and to configure the Linux
# environment.
#
# Run using:
# $ source install.sh
#
# Author: Danylo Malyuta, 2020.

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

# ..:: Copy dotfiles to home directory ::..

# Gnome desktop
mkdir -p ~/.config/gtk-3.0
ln -sf "$DIR"/.config/gtk-3.0/gtk.css ~/.config/gtk-3.0/
ln -sf "$DIR"/.xbindkeysrc ~

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

# Rofi desktop search
mkdir -p ~/.config/rofi
ln -sf "$DIR"/.config/rofi/config.rasi ~/.config/rofi/config.rasi

# Input devices
ln -sf "$DIR"/.input_config.sh ~
sudo ln -sf "$DIR"/.peripherals/40-libinput.conf /usr/share/X11/xorg.conf.d
sudo ln -sf "$DIR"/.peripherals/51-synaptics-quirks.conf /usr/share/X11/xorg.conf.d
sudo ln -sf "$DIR"/.peripherals/70-synaptics.conf /usr/share/X11/xorg.conf.d

# Python
cp -r "$DIR"/.jupyter ~
ln -sf "$DIR"/.config/flake8 ~/.config
ln -sf "$DIR"/.ipython/profile_default/startup/fzf_backward_search.py \
   ~/.ipython/profile_default/startup

# Julia
mkdir -p ~/.julia/config
ln -sf "$DIR"/.config/startup.jl ~/.julia/config

# Emacs
mkdir -p ~/.emacs.d
mkdir -p ~/.emacs.d/lisp
ln -sf "$DIR"/.emacs.d/init.el ~/.emacs.d
ln -sf "$DIR"/.emacs.d/lisp/danylo-custom-variables.el ~/.emacs.d/lisp
ln -sf "$DIR"/.emacs.d/lisp/danylo-common-font-lock.el ~/.emacs.d/lisp
ln -sf "$DIR"/.emacs.d/lisp/danylo-prog-font-lock.el ~/.emacs.d/lisp
ln -sf "$DIR"/.emacs.d/lisp/danylo-text-font-lock.el ~/.emacs.d/lisp
ln -sf "$DIR"/.emacs.d/lisp/snippets ~/.emacs.d/lisp/

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
