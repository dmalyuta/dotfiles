#!/bin/bash
# ----------------------------------------------------------------------
#
# Installation of Eclipse IDE.
#
# ----------------------------------------------------------------------

echo_prefix_temp="$echo_prefix"
echo_prefix="[dev tools setup] "

# Build tools like gcc, g++
apt_get_install_pkg build-essential

# Java 8
apt_get_install_pkg python-software-properties
runcmd "add-apt-repository ppa:webupd8team/java -y"
runcmd "apt-get update"
apt_get_install_pkg oracle-java8-installer 
apt_get_install_pkg oracle-java8-set-default

# Terminator
# Terminal emulator
runcmd "add-apt-repository ppa:gnome-terminator -y"
runcmd "apt-get update"
apt_get_install_pkg terminator
apt_get_install_pkg aptitude
apt_get_install_pkg xclip
apt_get_install_pkg silversearcher-ag
apt_get_install_pkg screen
apt_get_install_pkg htop
apt_get_install_pkg sshpass
apt_get_install_pkg tree
apt_get_install_pkg bash-completion
#apt_get_install_pkg rxvt-unicode

# tmux (version 2.3)
# Terminal multiplexer
if [ "$(tmux -V)" != "tmux 2.3" ]; then
    apt_get_install_pkg libevent-dev
    apt_get_install_pkg libncurses5-dev
    wget_targz_install "tmux-2.3" "https://github.com/tmux/tmux/releases/download/2.3/tmux-2.3.tar.gz"
fi

# .bashrc personal inclusions
source_local_bashrc=false
if ! grep -q ".local.bashrc" "${home}/.bashrc"; then
    source_local_bashrc=true
fi
if $source_local_bashrc; then
    # source .local.bashrc
    runcmd "eval builtin echo \"\" >> ${home}/.bashrc" nonull
    runcmd "eval builtin echo \"# personal additions to .bashrc\" >> ${home}/.bashrc" nonull
    runcmd "eval builtin echo \"if [ -f ~/.local.bashrc ]; then\" >> ${home}/.bashrc" nonull
    runcmd "eval builtin echo \"    . ~/.local.bashrc\" >> ${home}/.bashrc" nonull
    runcmd "eval builtin echo \"fi\" >> ${home}/.bashrc" nonull
fi

# Fix Unity bug that Ctrl-Alt-T creates a new icon in the Unity Dash
runcmd "gsettings set org.gnome.desktop.default-applications.terminal exec 'terminator'"

# Install QtCreator
if determine_install_with_type "qmake" "yN"; then
    runcmd "wget http://download.qt.io/archive/qt/5.7/5.7.0/qt-opensource-linux-x64-5.7.0.run -P ${home}/Downloads/"
    runcmd "chmod +x ${home}/Downloads/qt-opensource-linux-x64-5.7.0.run"
    runcmd "${home}/Downloads/qt-unified-linux-x64-online.run"
    runcmd "rm -f ${home}/Downloads/qt-opensource-linux-x64-5.7.0.run"
fi

# Install ROS plugin for QtCreator
runcmd "add-apt-repository ppa:levi-armstrong/qt-libraries-trusty -y"
runcmd "add-apt-repository ppa:levi-armstrong/ppa -y"
runcmd "apt-get update"
apt_get_install_pkg qt57creator-plugin-ros



echo_prefix="$echo_prefix_temp"
