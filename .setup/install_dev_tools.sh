#!/bin/bash
# ----------------------------------------------------------------------
#
# Installation of Eclipse IDE.
#
# ----------------------------------------------------------------------

echo_prefix_temp="$echo_prefix"
echo_prefix="[dev tools setup] "

apt_get_install_pkg build-essential
apt_get_install_pkg default-jre

# Install Eclipse Kepler
runcmd "wget http://www.eclipse.org/downloads/download.php?file=/technology/epp/downloads/release/kepler/SR2/eclipse-cpp-kepler-SR2-linux-gtk-x86_64.tar.gz&r=1 -O ${home}/Downloads/eclipse.tar.gz"
runcmd "tar zxf ${home}/Downloads/eclipse.tar.gz -C ${home}/Downloads/"
copy_foo "eclipse" "${home}/Downloads" "${home}/.eclipse"
runcmd "rm -f ${home}/Downloads/eclipse.tar.gz"

# Install Modelio
# (UML software)
runcmd "wget https://sourceforge.net/projects/modeliouml/files/3.6.1/modelio-open-source-3.6.1_amd64.deb/download -O ${home}/Downloads/modelio.deb"
runcmd "dpkg -i ${home}/Downloads/modelio.deb"
runcmd "apt-get --assume-yes -f install"
runcmd "rm -f ${home}/Downloads/modelio.deb"


# Install QtCreator
#runcmd "wget http://download.qt.io/archive/qt/5.7/5.7.0/qt-opensource-linux-x64-5.7.0.run -P ${home}/Downloads/"
# runcmd "chmod +x ${home}/Downloads/qt-opensource-linux-x64-5.7.0.run"
# runcmd "${home}/Downloads/qt-opensource-linux-x64-5.7.0.run"
# runcmd "rm -f ${home}/Downloads/qt-opensource-linux-x64-5.7.0.run"

# Install ROS plugin for QtCreator
# runcmd "add-apt-repository ppa:levi-armstrong/qt-libraries-trusty -y"
# runcmd "add-apt-repository ppa:levi-armstrong/ppa -y"
# runcmd "apt-get update"
# apt_get_install_pkg qt57creator-plugin-ros


echo_prefix="$echo_prefix_temp"
