#!/bin/bash
# ----------------------------------------------------------------------
#
# Installation of Eclipse IDE.
#
# ----------------------------------------------------------------------

echo_prefix_temp="$echo_prefix"
echo_prefix="[dev tools setup] "

# Install Java
runcmd "add-apt-repository ppa:webupd8team/java -y"
runcmd "apt-get update"

apt_get_install_pkg oracle-java8-installer nonull

# Install QtCreator
runcmd "wget http://download.qt.io/archive/qt/5.7/5.7.0/qt-opensource-linux-x64-5.7.0.run -P ${home}/Downloads/"
runcmd "chmod +x ${home}/Downloads/qt-opensource-linux-x64-5.7.0.run"
runcmd "${home}/Downloads/qt-unified-linux-x64-online.run"
runcmd "rm -f ${home}/Downloads/qt-opensource-linux-x64-5.7.0.run"

# Install ROS plugin for QtCreator
runcmd "add-apt-repository ppa:levi-armstrong/qt-libraries-trusty -y"
runcmd "add-apt-repository ppa:levi-armstrong/ppa -y"
runcmd "apt-get update"
apt_get_install_pkg qt57creator-plugin-ros

# Install StarUML
# UML modelling software
runcmd "wget http://staruml.io/download/release/v2.8.0/StarUML-v2.8.0-64-bit.deb -P ${home}/Downloads/"
runcmd "dpkg -i ${home}/Downloads/StarUML-v2.8.0-64-bit.deb"
runcmd "apt-get --assume-yes install -f"
runcmd "rm -f ${home}/Downloads/StarUML-v2.8.0-64-bit.deb"

echo_prefix="$echo_prefix_temp"
