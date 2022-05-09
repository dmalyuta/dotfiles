#!/bin/bash
#
# Javascript programming tools.
#
# Author: Danylo Malyuta, 2022.

# Install Node.js 16.x (JavaScript runtime)
if not_installed node; then
    curl -sL https://deb.nodesource.com/setup_16.x -o /tmp/nodesource_setup.sh
    sudo apt-get install -y nodejs
    sudo npm install --global yarn
fi
