#!/bin/bash
# ----------------------------------------------------------------------
#
# Installation of creative applications for drawing and diagramming.
#
# ----------------------------------------------------------------------

echo_prefix_temp="$echo_prefix"
echo_prefix="[creative setup] "

# Inkscape
# Vector graphics
if program_not_installed "inkscape"; then
    runcmd "add-apt-repository ppa:inkscape.dev/stable -y"
    runcmd "apt-get update"
    apt_get_install_pkg inkscape
fi

# yEd
# Diagramming
if [ ! -f "${home}/.yed/yed.jar" ]; then
    runcmd "wget https://www.yworks.com/resources/yed/demo/yEd-3.17.zip -O /tmp/yEd.zip"
    runcmd "rm -rf /tmp/.yed"
    runcmd "mkdir -p ${home}/.yed"
    runcmd "unzip /tmp/yEd.zip -d ${home}/.yed/"
    runcmd "mv ${home}/.yed/yed-3.17/* ${home}/.yed/"
    runcmd "rm -rf ${home}/.yed/yed-3.17"
    runcmd "eval chown -R ${SUDO_USER:-$USER}:${SUDO_USER:-$USER} ${home}/.yed"
fi

# Dia
# Diagramming
if program_not_installed "dia"; then
    runcmd "wget https://downloads.sourceforge.net/project/dia-installer/dia/0.97.2/dia_0.97.2-5_i386.deb?r=http%3A%2F%2Fdia-installer.de%2Fdownload%2Flinux.html&ts=1493486541&use_mirror=iweb -O /tmp/dia.deb"
    runcmd_noexit "dpkg -i /tmp/dia.deb" nonull
    runcmd "apt-get --assume-yes install -f" nonull
fi

echo_prefix="$echo_prefix_temp"
