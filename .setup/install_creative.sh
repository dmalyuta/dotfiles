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
    runcmd "wget https://www.yworks.com/resources/yed/demo/yEd-3.17.zip -O ${home}/Downloads/yEd.zip"
    runcmd "rm -rf ${home}/Downloads/.yed"
    runcmd "mkdir -p ${home}/.yed"
    runcmd "unzip ${home}/Downloads/yEd.zip -d ${home}/.yed/"
    runcmd "mv ${home}/.yed/yed-3.17/* ${home}/.yed/"
    runcmd "rm -rf ${home}/.yed/yed-3.17"
    runcmd "eval chown -R ${SUDO_USER:-$USER}:${SUDO_USER:-$USER} ${home}/.yed"
    runcmd "rm -rf ${home}/Downloads/yEd.zip"
fi

# Dia
# Diagramming
if program_not_installed "dia"; then
    runcmd "wget https://downloads.sourceforge.net/project/dia-installer/dia/0.97.2/dia_0.97.2-5_i386.deb?r=http%3A%2F%2Fdia-installer.de%2Fdownload%2Flinux.html&ts=1493486541&use_mirror=iweb -O ${home}/Downloads/dia.deb"
    runcmd_noexit "dpkg -i ${home}/Downloads/dia.deb" nonull
    runcmd "apt-get --assume-yes install -f" nonull
    runcmd "rm -f ${home}/Downloads/dia.deb"
fi

# Draw.io Desktop
# Diagramming
#
# Follow up this installation with the following actions:
#     - Install the Draw.io Desktop extension: https://chrome.google.com/webstore/detail/drawio-desktop/pebppomjfocnoigkeepgbmcifnnlndla/related?hl=en-GB
if program_not_installed "google-chrome"; then
    echowarn "Please read the instructions in comments of .setup/install_creative.sh for follow-up installation actions inside Google Chrome!"
    apt_get_install_pkg libxss1
    apt_get_install_pkg libappindicator1
    apt_get_install_pkg libindicator7
    runcmd "wget https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb -O ${home}/Downloads/chrome.deb"
    runcmd_noexit "dpkg -i ${home}/Downloads/chrome.deb" nonull
    runcmd "apt-get --assume-yes install -f" nonull
    runcmd "rm -f ${home}/Downloads/chrome.deb"
fi

echo_prefix="$echo_prefix_temp"
