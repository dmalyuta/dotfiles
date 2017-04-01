#!/bin/bash
# ----------------------------------------------------------------------
#
# Installation of tools for software development.
#
# ----------------------------------------------------------------------

echo_prefix_temp="$echo_prefix"
echo_prefix="[dev tools setup] "





# Install R and RStudio
if determine_install_with_type "R and RStudio" "rstudio" "yN"; then
    # Install R
    runcmd "eval sh -c 'echo \"deb http://cran.rstudio.com/bin/linux/ubuntu trusty/\" >> /etc/apt/sources.list'"
    runcmd "eval su -c \"gpg --keyserver keyserver.ubuntu.com --recv-key E084DAB9\" ${SUDO_USER:-$USER}"
    runcmd "gpg -a --export E084DAB9 | sudo apt-key add -"
    runcmd "apt-get update"
    apt_get_install_pkg r-base

    # Install RStudio
    if ! program_not_installed "rstudio"; then
        runcmd "apt-get purge --assume-yes rstudio"
    fi
    runcmd "wget https://download1.rstudio.org/rstudio-1.0.136-amd64.deb -P ${home}/Downloads/"
    runcmd "dpkg -i ${home}/Downloads/rstudio-1.0.136-amd64.deb"
    runcmd "apt-get --assume-yes install -f"
    runcmd "rm -f ${home}/Downloads/rstudio-1.0.136-amd64.deb"
fi

# Install Astah (UML software)
if determine_install_with_type "Astah Professional" "astah-pro" "yN"; then
    if ! program_not_installed "astah-professional"; then
        runcmd "apt-get purge --assume-yes astah-professional"
    fi
    runcmd "wget http://cdn.astah.net/downloads/astah-professional_7.1.0.f2c212-0_all.deb -O ${home}/Downloads/astah.deb"
    runcmd "dpkg -i ${home}/Downloads/astah.deb" nonull
    runcmd "apt-get --assume-yes install -f"
    runcmd "rm -f ${home}/Downloads/astah.deb"
fi


echo_prefix="$echo_prefix_temp"
