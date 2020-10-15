#!/bin/bash
# ------------------------------------------------------------------------------
#
# Installation of tools for engineering and, in particular, software/algorithms
# development.
#
# ------------------------------------------------------------------------------

echo_prefix_temp="$echo_prefix"
echo_prefix="[dev tools setup] "

# Latest version of Git
git_version="$(git --version | cut -d ' ' -f 3 | cut -d '.' -f 1)"
if [ "$git_version" -lt 2 ]; then
    # Need to upgrade Git
    apt_get_install_pkg python-software-properties
    runcmd "add-apt-repository ppa:git-core/ppa -y"
    runcmd "apt-get update"
    apt_get_install_pkg git
fi

# Gitk
# Visual viewer of git history
apt_get_install_pkg gitk

# Build tools like gcc, g++
apt_get_install_pkg build-essential

# Google logging library
apt_get_install_pkg libgoogle-glog-dev

# gdbserver
# Remote debugging
apt_get_install_pkg gdbserver

# Terminator (replace Gnome Terminal with Terminator)
# Terminal emulator
if program_not_installed "terminator"; then
    #runcmd "add-apt-repository ppa:gnome-terminator -y"
    #runcmd "apt-get update"
    apt_get_install_pkg terminator

    # Make Terminator the new Gnome Terminal!
    runcmd "apt-get purge -y gnome-terminal"
    runcmd "ln -s /usr/bin/terminator /usr/bin/gnome-terminal"
fi

# Cmake 3.18.4
if function_not_defined "cmake"; then
    runcmd "wget -4 https://github.com/Kitware/CMake/releases/download/v3.18.4/cmake-3.18.4.tar.gz -O /tmp/cmake.tar.gz"
    runcmd_noexit "tar -xvf /tmp/cmake.tar.gz -C /tmp" nonull
    (runcmd "cd /tmp/cmake/" && runcmd "./bootstrap" && runcmd "make" && runcmd "make install")
fi

# Other command line utilities
apt_get_install_pkg aptitude
apt_get_install_pkg xclip
apt_get_install_pkg silversearcher-ag
apt_get_install_pkg screen
apt_get_install_pkg htop
apt_get_install_pkg sshpass
apt_get_install_pkg tree
apt_get_install_pkg bash-completion
#apt_get_install_pkg rxvt-unicode

# Meld Diff Viewer
# It is a GUI analog to the standard diff install diffutils and patch install patch command line tools
apt_get_install_pkg meld

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

echo_prefix="$echo_prefix_temp"
