#!/bin/bash
# ----------------------------------------------------------------------
#
# i3 window manager installation.
#
# i3 is a tiling window manager. The target platforms are GNU/Linux
# and BSD operating systems, the code is Free and Open Source Software
# (FOSS) under the BSD license. i3 is primarily targeted at advanced
# users and developers.
#
# To learn more about i3, visit: https://i3wm.org/
#
# ----------------------------------------------------------------------

echo_prefix_temp="$echo_prefix"
echo_prefix="[i3 setup] "

# i3 itself

apt_get_install_pkg i3
apt_get_install_pkg i3status
apt_get_install_pkg i3lock
apt_get_install_pkg feh
apt_get_install_pkg sysstat
apt_get_install_pkg acpi
apt_get_install_pkg compton

# i3blocks (status bar info like volume, battery, etc.)

if program_not_installed "i3blocks"; then
    if [[ "$os_name" == "Ubuntu" ]] && [[ ${os_version:0:2} -lt 15 ]]; then
	# install from source
	runcmd "wget https://github.com/vivien/i3blocks/releases/download/1.4/i3blocks-1.4.tar.gz"
	runcmd "tar -zxvf i3blocks-1.4.tar.gz"
	(runcmd "cd i3blocks-1.4/" && runcmd "make clean all" && runcmd "make install LIBEXECDIR=/usr/share")
	subshell_check $?
	runcmd "rm -rf i3blocks*"
    else
	# install i3blocks with apt-get install
	apt_get_install_pkg i3blocks
    fi
fi

# numix theme and icons

runcmd "add-apt-repository -y ppa:numix/ppa"
runcmd "apt-get update"
apt_get_install_pkg numix-gtk-theme
apt_get_install_pkg numix-icon-theme

# arc-theme

# if ! dpkg -l "arc-theme" >/dev/null 2>&1; then
#     # arc-theme not installed --> install it
#     # following instructions: http://software.opensuse.org/download.html?project=home%3AHorst3180&package=arc-theme
    
#     runcmd "wget http://download.opensuse.org/repositories/home:Horst3180/xUbuntu_16.04/Release.key"
#     runcmd "apt-key add - < Release.key"
#     runcmd "rm Release.key"
#     runcmd "apt-get update"
#     runcmd "sh -c \"echo 'deb http://download.opensuse.org/repositories/home:/Horst3180/xUbuntu_16.04/ /' > /etc/apt/sources.list.d/arc-theme.list\""
#     runcmd "apt-get update"
#     apt_get_install_pkg arc-theme
# fi

# playerctl

if program_not_installed "playerctl"; then
    # playerctl not installed --> install it

    runcmd "wget https://github.com/acrisci/playerctl/releases/download/v0.5.0/playerctl-0.5.0_amd64.deb"
    runcmd "dpkg -i playerctl-0.5.0_amd64.deb"
    runcmd "rm -rf playerctl-0.5.0_amd64.deb"
fi

# rofi

if program_not_installed "rofi"; then
    # rofi not installed --> install it

    # install dependencies
    if [[ "$os_name" == "Ubuntu" ]] && [[ ${os_version:0:2} -lt 15 ]]; then
	# workaround for Ubuntu 14.04 for installing xkbcommon berion 0.5
	# see https://github.com/DaveDavenport/rofi/issues/371
	
	runcmd "eval add-apt-repository -y 'deb http://debian.jpleau.ca/ jessie-backports main contrib non-free'"
	runcmd "eval apt-key adv --keyserver keyserver.ubuntu.com --recv-keys ED45C181B540212D"
	echowarn "Installed potentially missing public key ED45C181B540212D. In case this key has changed and the apt-get update command below throws a NO_PUBKEY error, re-run the above command manually, replacing the public key value with the one mentioned in the error."
	runcmd "sudo apt-get update -qq"
	runcmd "sudo apt-get install -y libxkbcommon-dev libxkbcommon-x11-dev"
    fi
    
    apt_get_install_pkg libpango-1.0-0
    apt_get_install_pkg libpangocairo-1.0-0
    apt_get_install_pkg libcairo2
    apt_get_install_pkg xcb
    apt_get_install_pkg libxcb-util0-dev
    apt_get_install_pkg libxcb-ewmh-dev
    apt_get_install_pkg libxcb-xinerama0
    apt_get_install_pkg libxcb-xinerama0-dev
    apt_get_install_pkg libxcb-icccm4-dev
    apt_get_install_pkg libxcb-xkb-dev
    apt_get_install_pkg libstartup-notification0
    apt_get_install_pkg libstartup-notification0-dev
    apt_get_install_pkg libxkbcommon-x11-dev	
    #apt_get_install_pkg libxkbcommon0
    apt_get_install_pkg libxkbcommon-dev
    apt_get_install_pkg libglib2.0-dev
    apt_get_install_pkg libperl-dev
    apt_get_install_pkg libgtk2.0-dev
    apt_get_install_pkg xutils
    apt_get_install_pkg xutils-dev
    apt_get_install_pkg m4

    # install xcb-util-xrm dependency
    # TODO: somehow check if installed already
    
    wget_targz_install "xcb-util-xrm-1.0" "https://github.com/Airblader/xcb-util-xrm/releases/download/v1.0/xcb-util-xrm-1.0.tar.gz"
    
    # install rofi

    wget_targz_install "rofi-1.2.0" "https://github.com/DaveDavenport/rofi/releases/download/1.2.0/rofi-1.2.0.tar.gz"
    
    # make sure that shared libraries in /usr/local/lib/ are seen
    
    if [ ! -f  "/etc/ld.so.conf.d/usr-local.conf" ]; then
	runcmd "touch /etc/ld.so.conf.d/usr-local.conf"
    fi
    runcmd "echo \"/usr/local/lib\" > /etc/ld.so.conf.d/usr-local.conf"
    runcmd "ldconfig"
fi

# if OS is Ubuntu, make sure Nautilus file manager does not make Desktop pop up when launched

if [[ "$os_name" == "Ubuntu" ]]; then
    runcmd "gsettings set org.gnome.desktop.background show-desktop-icons false"
fi

echo_prefix="$echo_prefix_temp"
