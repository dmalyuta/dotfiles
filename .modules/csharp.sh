#!/bin/bash
#
# C# programming tools.
#
# Author: Danylo Malyuta, 2022.

SDK_VERSION="6.0.101"
SDK_APT_INSTALL_VERSION="6.0.101-1" # see available versions in apt with "apt-cache madison dotnet-sdk-6.0"

# Check if dotnet is already installed
which_dotnet=$(which dotnet || true)
if [ -z "$which_dotnet" ]
then
    echo "dotnet runtime 6.0.x not installed. Installing."
else
    echo "Detected dotnet already installed. Checking for desired version."
    installed_runtime=$(dotnet --list-runtimes | grep "Microsoft.NETCore.App 6.0" || true)
    if [ -z "$installed_runtime" ]
    then
        echo "dotnet runtime 6.0.x not installed. Installing."
    else
        echo "Detected dotnet runtime 6.0.x installed. Skipping."
        exit 0
    fi
fi

# Install .NET 6 SDK for Linux, targeting Ubuntu 16.04
# See https://docs.microsoft.com/en-us/dotnet/core/install/linux-ubuntu#1604-
wget https://packages.microsoft.com/config/ubuntu/20.04/packages-microsoft-prod.deb \
    -O /tmp/packages-microsoft-prod.deb
sudo dpkg -i /tmp/packages-microsoft-prod.deb
sudo apt update -y
sudo apt install -y apt-transport-https
sudo apt update -y
sudo apt install -y dotnet-sdk-6.0=${SDK_APT_INSTALL_VERSION}
rm /tmp/packages-microsoft-prod.deb

# Source dotnet tools folder
mkdir -p ~/.dotnet/tools
cat << EOF >> ~/.bashrc
export PATH=\$PATH:~/.dotnet/tools
EOF
