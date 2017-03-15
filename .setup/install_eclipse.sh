#!/bin/bash
# ----------------------------------------------------------------------
#
# Installation of Eclipse IDE.
#
# ----------------------------------------------------------------------

echo_prefix_temp="$echo_prefix"
echo_prefix="[eclipse setup] "

# Install Java
runcmd "add-apt-repository ppa:webupd8team/java -y"
runcmd "apt-get update"

apt_get_install_pkg oracle-java8-installer nonull

# Install Eclipse CDT
eclipse_name="eclipse-cpp-neon-2-linux-gtk-x86_64"
eclipse_mirror="http://mirror.csclub.uwaterloo.ca/eclipse/technology/epp/downloads/release/neon/2/${eclipse_name}.tar.gz"
runcmd "wget $eclipse_mirror -P ${home}/Downloads/"
runcmd "tar -zxvf ${home}/Downloads/${eclipse_name}.tar.gz -C ${home}/Downloads/"
runcmd "rm -rf ${home}/Downloads/${eclipse_name}.tar.gz"
runcmd "mkdir -p ${home}/.eclipse/"
runcmd "mv ${home}/Downloads/eclipse ${home}/.eclipse/"

echo "############################################"
echo "Done installing Eclipse"
echo "To keep synced settings across computers, a"
echo "good workflow is to store your workspace on"
echo "Dropbox."
echo "############################################"

echo_prefix="$echo_prefix_temp"
