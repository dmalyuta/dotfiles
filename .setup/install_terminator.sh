#!/bin/bash
# ----------------------------------------------------------------------
#
# Terminator installation.
#
# A terminal emulator supporting tabs and multiple resizable terminal
# panels in one window native based on GNOME Terminal
#
# To learn more about Terminator, visit:
# https://gnometerminator.blogspot.ch/p/introduction.html
#
# ----------------------------------------------------------------------

echo_prefix_temp="$echo_prefix"
echo_prefix="[terminator setup] "

# install terminator
runcmd "add-apt-repository ppa:gnome-terminator -y"
runcmd "apt-get update"
apt_get_install_pkg terminator
apt_get_install_pkg aptitude
apt_get_install_pkg xclip
apt_get_install_pkg silversearcher-ag
runcmd "apt-get --assume-yes install python3-dev python3-pip"
runcmd "sudo -H pip3 install thefuck"
apt_get_install_pkg screen
apt_get_install_pkg htop
apt_get_install_pkg sshpass
apt_get_install_pkg bash-completion

# tmux (version 2.3)
if [ "$(tmux -V)" != "tmux 2.3" ]; then
    apt_get_install_pkg libevent-dev
    wget_targz_install "tmux-2.3" "https://github.com/tmux/tmux/releases/download/2.3/tmux-2.3.tar.gz"
fi

# update .bashrc to include an indication of which Git branch user is on
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







































