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
if determine_install "Eclipse CDT (Kepler)" "yN" "${home}/.eclipse/eclipse_cdt"; then
    runcmd "wget http://www.eclipse.org/downloads/download.php?file=/technology/epp/downloads/release/kepler/SR2/eclipse-cpp-kepler-SR2-linux-gtk-x86_64.tar.gz&r=1 -O ${home}/Downloads/eclipse_cdt.tar.gz"
    runcmd "rm -rf ${home}/Downloads/eclipse_cdt"
    runcmd "mkdir -p ${home}/Downloads/eclipse_cdt"
    runcmd "tar zxf ${home}/Downloads/eclipse_cdt.tar.gz --strip 1 -C ${home}/Downloads/eclipse_cdt"
    
    runcmd "mkdir -p ${home}/.eclipse"
    runcmd "rm -rf ${home}/.eclipse/eclipse_cdt"
    runcmd "mv ${home}/Downloads/eclipse_cdt ${home}/.eclipse/"
    runcmd "eval chown -R ${SUDO_USER:-$USER}:${SUDO_USER:-$USER} ${home}/.eclipse/eclipse_cdt"

    runcmd "rm -f ${home}/Downloads/eclipse_cdt.tar.gz"
    runcmd "desktop-file-install ${dir}/.eclipse/eclipse_cdt.desktop"
fi

# Eclipse Modeling Tools (Kepler)
# (UML software)
if determine_install "Eclipse Modeling Tools (Kepler)" "yN" "${home}/.eclipse/eclipse_modeling_tools"; then
    runcmd "wget http://www.eclipse.org/downloads/download.php?file=/technology/epp/downloads/release/kepler/SR2/eclipse-modeling-kepler-SR2-linux-gtk-x86_64.tar.gz&r=1 -O ${home}/Downloads/eclipse_modeling_tools.tar.gz"
    runcmd "rm -rf ${home}/Downloads/eclipse_modeling_tools"
    runcmd "mkdir -p ${home}/Downloads/eclipse_modeling_tools"
    runcmd "tar zxf ${home}/Downloads/eclipse_modeling_tools.tar.gz --strip 1 -C ${home}/Downloads/eclipse_modeling_tools"
    
    runcmd "mkdir -p ${home}/.eclipse"
    runcmd "rm -rf ${home}/.eclipse/eclipse_modeling_tools"
    runcmd "mv ${home}/Downloads/eclipse_modeling_tools ${home}/.eclipse/"
    runcmd "eval chown -R ${SUDO_USER:-$USER}:${SUDO_USER:-$USER} ${home}/.eclipse/eclipse_modeling_tools"
    
    runcmd "rm -f ${home}/Downloads/eclipse_modeling_tools.tar.gz"
    runcmd "desktop-file-install ${dir}/.eclipse/eclipse_modeling_tools.desktop"
fi


echo_prefix="$echo_prefix_temp"
