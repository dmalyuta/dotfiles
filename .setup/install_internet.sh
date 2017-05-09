#!/bin/bash
# ----------------------------------------------------------------------
#
# Installation of Internet/Web applications.
#
# ----------------------------------------------------------------------

echo_prefix_temp="$echo_prefix"
echo_prefix="[internet setup] "

# Google Chrome
if program_not_installed "google-chrome"; then
    apt_get_install_pkg libxss1
    apt_get_install_pkg libappindicator1
    apt_get_install_pkg libindicator7
    runcmd "wget https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb -O ${home}/Downloads/chrome.deb"
    runcmd_noexit "dpkg -i ${home}/Downloads/chrome.deb" nonull
    runcmd "apt-get --assume-yes install -f" nonull
    runcmd "rm -f ${home}/Downloads/chrome.deb"
fi

# Google Drive client for the commandline
# https://github.com/odeke-em/drive
if [ ! -f "${home}/gopath/bin/drive" ]; then
	# Install the Go programming language
	if [ ! -d "/usr/local/go" ]; then
		runcmd "wget https://storage.googleapis.com/golang/go1.7.4.linux-amd64.tar.gz -O ${home}/Downloads/go_language.tar.gz"
		runcmd "tar zxf ${home}/Downloads/go_language.tar.gz -C ${home}/Downloads/"
		runcmd "eval chown -R ${SUDO_USER:-$USER}:${SUDO_USER:-$USER} ${home}/Downloads/go"
		runcmd "mv ${home}/Downloads/go /usr/local"
		runcmd "rm -f ${home}/Downloads/go_language.tar.gz"
		# Make path for Go
		runcmd "mkdir -p ${home}/gopath"
		runcmd "eval chown -R ${SUDO_USER:-$USER}:${SUDO_USER:-$USER} ${home}/gopath"
	fi
	# Install Google Drive client
	runcmd "eval su -c \"go get -u github.com/odeke-em/drive/cmd/drive\" ${SUDO_USER:-$USER}"
fi

# Insync Google Drive client
if program_not_installed "insync"; then
	runcmd "wget https://d2t3ff60b2tol4.cloudfront.net/builds/insync_1.3.16.36155-trusty_amd64.deb -O ${home}/Downloads/insync.deb"
    runcmd_noexit "dpkg -i ${home}/Downloads/insync.deb" nonull
    runcmd "apt-get --assume-yes install -f" nonull
    runcmd "rm -f ${home}/Downloads/insync.deb"
fi

echo_prefix="$echo_prefix_temp"
