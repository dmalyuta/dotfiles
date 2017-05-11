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
	runcmd "eval su -c \"/usr/local/go/bin/go get -u github.com/odeke-em/drive/cmd/drive\" ${SUDO_USER:-$USER}"
fi

# Rclone
# "rsync for cloud storage"
# Rclone is a command line program to sync files and directories to and from cloud services (Google Drive, Dropbox, etc.)
# Installation instructions taken from https://rclone.org/install/
if program_not_installed "rclone"; then
	# Fetch and unpack
	runcmd "wget https://downloads.rclone.org/rclone-current-linux-amd64.zip -O ${home}/Downloads/rclone.zip"
	runcmd "unzip ${home}/Downloads/rclone.zip -d ${home}/Downloads/"
	# Copy binary file
	runcmd "cp ${home}/Downloads/rclone-*-linux-amd64/rclone /usr/bin/"
	runcmd "chown root:root /usr/bin/rclone"
	runcmd "chmod 755 /usr/bin/rclone"
	# Install manpage
	runcmd "mkdir -p /usr/local/share/man/man1"
	runcmd "cp ${home}/Downloads/rclone-*-linux-amd64/rclone.1 /usr/local/share/man/man1/"
	runcmd "mandb"
	runcmd "rm -rf ${home}/Downloads/rclone*"
fi

echo_prefix="$echo_prefix_temp"
