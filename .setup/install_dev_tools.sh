#!/bin/bash
# ----------------------------------------------------------------------
#
# Installation of Eclipse IDE.
#
# ----------------------------------------------------------------------

echo_prefix_temp="$echo_prefix"
echo_prefix="[dev tools setup] "

apt_get_install_pkg build-essential
apt_get_install_pkg default-jre

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

# Eclipse CDT (Kepler)
# C/C++ programming
runcmd "wget http://www.eclipse.org/downloads/download.php?file=/technology/epp/downloads/release/kepler/SR2/eclipse-cpp-kepler-SR2-linux-gtk-x86_64.tar.gz&r=1 -O ${home}/Downloads/eclipse.tar.gz"
runcmd "tar zxf ${home}/Downloads/eclipse.tar.gz -C ${home}/Downloads/"
copy_foo "eclipse" "${home}/Downloads" "${home}/.eclipse"
runcmd "rm -f ${home}/Downloads/eclipse.tar.gz"

# Eclipse Modeling Tools (Kepler)
# (UML software)
runcmd "wget http://www.eclipse.org/downloads/download.php?file=/technology/epp/downloads/release/kepler/SR2/eclipse-modeling-kepler-SR2-linux-gtk-x86_64.tar.gz&r=1 -O ${home}/Downloads/eclipse_modeling_tools.tar.gz"
runcmd "rm -rf ${home}/Downloads/eclipse_modeling_tools"
runcmd "mkdir -p ${home}/Downloads/eclipse_modeling_tools"
runcmd "tar zxf ${home}/Downloads/eclipse_modeling_tools.tar.gz --strip 1 -C ${home}/Downloads/eclipse_modeling_tools"
copy_foo "eclipse_modeling_tools" "${home}/Downloads" "${home}/.eclipse"
runcmd "rm -f ${home}/Downloads/eclipse_modeling_tools.tar.gz"


echo_prefix="$echo_prefix_temp"
