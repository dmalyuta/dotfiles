#!/bin/bash
# ----------------------------------------------------------------------
#
# Installation of Internet/Web applications.
#
# ----------------------------------------------------------------------

echo_prefix_temp="$echo_prefix"
echo_prefix="[internet setup] "

# # Google Chrome
# if program_not_installed "google-chrome"; then
#     apt_get_install_pkg libxss1
#     apt_get_install_pkg libappindicator1
#     apt_get_install_pkg libindicator7
#     runcmd "wget https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb -O /tmp/chrome.deb"
#     runcmd_noexit "dpkg -i /tmp/chrome.deb" nonull
#     runcmd "apt-get --assume-yes install -f" nonull
# fi

# # Dropbox
# if program_not_installed "dropbox"; then
#     runcmd "wget https://www.dropbox.com/download?dl=packages/ubuntu/dropbox_2015.10.28_amd64.deb -O /tmp/dropbox.deb"
#     runcmd_noexit "dpkg -i /tmp/dropbox.deb" nonull
#     runcmd "apt-get --assume-yes install -f" nonull
# fi

# # Mailspring
# if program_not_installed "mailspring"; then
#     apt_get_install_pkg libsecret-1-dev
#     apt_get_install_pkg gir1.2-gnomekeyring-1.0
#     runcmd "wget https://updates.getmailspring.com/download?platform=linuxDeb -O /tmp/mailspring.deb"
#     runcmd_noexit "dpkg -i /tmp/mailspring.deb" nonull
#     runcmd "apt-get --assume-yes install -f" nonull
# fi

# Lastpass CLI
if program_not_installed "lpass"; then
    apt_get_install_pkg bash-completion
    apt_get_install_pkg build-essential
    apt_get_install_pkg libcurl4
    apt_get_install_pkg libcurl4-openssl-dev
    apt_get_install_pkg libssl-dev
    apt_get_install_pkg libxml2
    apt_get_install_pkg libxml2-dev
    apt_get_install_pkg libssl1.1
    apt_get_install_pkg pkg-config
    apt_get_install_pkg ca-certificates
    apt_get_install_pkg xclip
    apt_get_install_pkg lastpass-cli
fi

# Networking tools
apt_get_install_pkg net-tools


echo_prefix="$echo_prefix_temp"
