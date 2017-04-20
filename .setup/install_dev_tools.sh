#!/bin/bash
# ----------------------------------------------------------------------
#
# Installation of tools for engineering and, in particular, software
# development.
#
# ----------------------------------------------------------------------

echo_prefix_temp="$echo_prefix"
echo_prefix="[dev tools setup] "

# Build tools like gcc, g++
apt_get_install_pkg build-essential

# Java 8
java_version=$(java -version 2>&1 | grep "java version")
if echo "$java_version" | grep "1.8" &>/dev/null; then # TODO change to a check whether version is *at least* 1.8 (e.g. 1.9 is OK, don't do anything then)
    apt_get_install_pkg python-software-properties
    runcmd "add-apt-repository ppa:webupd8team/java -y"
    runcmd "apt-get update"
    apt_get_install_pkg oracle-java8-installer nonull
    apt_get_install_pkg oracle-java8-set-default
fi

# Terminator
# Terminal emulator
if program_not_installed "terminator"; then
    runcmd "add-apt-repository ppa:gnome-terminator -y"
    runcmd "apt-get update"
    apt_get_install_pkg terminator
fi

# Other command line utilities
apt_get_install_pkg aptitude
apt_get_install_pkg xclip
apt_get_install_pkg silversearcher-ag
apt_get_install_pkg screen
apt_get_install_pkg htop
apt_get_install_pkg sshpass
apt_get_install_pkg tree
apt_get_install_pkg bash-completion
#apt_get_install_pkg rxvt-unicode

# tmux (version 2.3)
# Terminal multiplexer
if [ "$(tmux -V)" != "tmux 2.3" ]; then
    apt_get_install_pkg libevent-dev
    apt_get_install_pkg libncurses5-dev
    wget_targz_install "tmux-2.3" "https://github.com/tmux/tmux/releases/download/2.3/tmux-2.3.tar.gz"
fi

# .bashrc personal inclusions
source_local_bashrc=false
if ! grep -q ".local.bashrc" "${home}/.bashrc"; then
    source_local_bashrc=true
fi
if $source_local_bashrc; then
    # source .local.bashrc
    runcmd "eval builtin echo \"\" >> ${home}/.bashrc" nonull
    runcmd "eval builtin echo \"# personal additions to .bashrc\" >> ${home}/.bashrc" nonull
    runcmd "eval builtin echo \"if [ -f ~/.local.bashrc ]; then\" >> ${home}/.bashrc" nonull
    runcmd "eval builtin echo \"    . ~/.local.bashrc\" >> ${home}/.bashrc" nonull
    runcmd "eval builtin echo \"fi\" >> ${home}/.bashrc" nonull
fi

# Fix Unity bug that Ctrl-Alt-T creates a new icon in the Unity Dash
runcmd "gsettings set org.gnome.desktop.default-applications.terminal exec 'terminator'"

# Install Wine pre-requisites for running Windows programs on Linux
# In particular, I use it to run [Arbre Analyste](http://www.arbre-analyste.fr/)
if program_not_installed "winetricks"; then
    runcmd "dpkg --add-architecture i386"
    runcmd "add-apt-repository ppa:wine/wine-builds -y"
    runcmd "apt-get update"
    runcmd "apt-get install --assume-yes --install-recommends winehq-devel"
    apt_get_install_pkg winetricks
fi


####### Programs below are installed *only* if they are not already installed

# Install Haroopad (Markdown editor)
if program_not_installed "haroopad"; then
    runcmd "wget https://bitbucket.org/rhiokim/haroopad-download/downloads/haroopad-v0.13.1-x64.deb -O ${home}/Downloads/haroopad.deb"
    runcmd_noexit "dpkg -i ${home}/Downloads/haroopad.deb" nonull
    runcmd "apt-get --assume-yes install -f" nonull
    runcmd "rm -f ${home}/Downloads/haroopad.deb"
fi

# Install SmartGit
if ! dpkg -l | grep -E '^ii' | grep smartgit &>/dev/null; then
    runcmd "wget http://www.syntevo.com/static/smart/download/smartgit/smartgit-17_0_3.deb -O ${home}/Downloads/smartgit.deb"
    runcmd_noexit "dpkg -i ${home}/Downloads/smartgit.deb" nonull
    runcmd "apt-get --assume-yes install -f"
    runcmd "rm -f ${home}/Downloads/smartgit.deb"
fi

# Install Jetbrains CLion (C/C++)
if [ ! -d "${home}/.jetbrains/clion" ]; then
    runcmd "wget https://download.jetbrains.com/cpp/CLion-2017.1.tar.gz -O ${home}/Downloads/clion.tar.gz"
    runcmd "rm -rf ${home}/.jetbrains/clion"
    runcmd "mkdir -p ${home}/.jetbrains/clion"
    runcmd "tar zxf ${home}/Downloads/clion.tar.gz --strip 1 -C ${home}/.jetbrains/clion"
    runcmd "eval chown -R ${SUDO_USER:-$USER}:${SUDO_USER:-$USER} ${home}/.jetbrains/clion"
    runcmd "rm -f ${home}/Downloads/clion.tar.gz"
fi

# Install Jetbrains PyCharm (Python)
if [ ! -d "${home}/.jetbrains/pycharm" ]; then
    runcmd "wget https://download.jetbrains.com/python/pycharm-professional-2017.1.1.tar.gz -O ${home}/Downloads/pycharm.tar.gz"
    runcmd "rm -rf ${home}/.jetbrains/pycharm"
    runcmd "mkdir -p ${home}/.jetbrains/pycharm"
    runcmd "tar zxf ${home}/Downloads/pycharm.tar.gz --strip 1 -C ${home}/.jetbrains/pycharm"
    runcmd "eval chown -R ${SUDO_USER:-$USER}:${SUDO_USER:-$USER} ${home}/.jetbrains/pycharm"
    runcmd "rm -f ${home}/Downloads/pycharm.tar.gz"
fi

# Install R and RStudio
if program_not_installed "rstudio"; then
    # Install R
    runcmd "eval sh -c 'echo \"deb http://cran.rstudio.com/bin/linux/ubuntu trusty/\" >> /etc/apt/sources.list'"
    runcmd "eval su -c \"gpg --keyserver keyserver.ubuntu.com --recv-key E084DAB9\" ${SUDO_USER:-$USER}"
    runcmd "gpg -a --export E084DAB9 | sudo apt-key add -"
    runcmd "apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9"
    runcmd "apt-get update"
    runcmd "apt-get install --assume-yes --force-yes r-base"

    # Install RStudio
    runcmd "wget https://download1.rstudio.org/rstudio-1.0.136-amd64.deb -P ${home}/Downloads/"
    runcmd_noexit "dpkg -i ${home}/Downloads/rstudio-1.0.136-amd64.deb"
    runcmd "apt-get --assume-yes install -f"
    runcmd "rm -f ${home}/Downloads/rstudio-1.0.136-amd64.deb"
fi

# Install Eclipse
#
# Follow up this installation with the following actions:
#   - Launch Eclipse: in the terminal run
#                     $ cdt
#   - Install XML editing (Oxygen XML plugin): Help --> Install New Software... --> Update site: http://www.oxygenxml.com/InstData/Editor/Eclipse/site.xml
#   - Install LaTeX editing (TeXlipse): Help --> Install New Software... --> Update site: http://texlipse.sourceforge.net
#   - Install integration with the terminal (EasyShell): Help --> Install New Software... --> Update site: http://anb0s.github.io/EasyShell
if [ ! -f "${home}/.eclipse/eclipse_cdt/eclipse" ]; then
    echowarn "Please read the instructions in comments of .setup/install_dev_tools.sh for follow-up installation actions inside Eclipse!"
    runcmd "wget \"https://www.eclipse.org/downloads/download.php?file=/technology/epp/downloads/release/neon/3/eclipse-cpp-neon-3-linux-gtk-x86_64.tar.gz&r=1\" -O ${home}/Downloads/eclipse_cdt.tar.gz"
    runcmd "mkdir -p ${home}/.eclipse/eclipse_cdt"
    runcmd "tar zxf ${home}/Downloads/eclipse_cdt.tar.gz --strip 1 -C ${home}/.eclipse/eclipse_cdt"
    runcmd "eval chown -R ${SUDO_USER:-$USER}:${SUDO_USER:-$USER} ${home}/.eclipse/eclipse_cdt"
    runcmd "rm -f ${home}/Downloads/eclipse_cdt.tar.gz"
fi

# Install Papyrus (OMG::UML and OMG::SysML software)
#
# Follow up this installation with the following actions:
#   - Launch Papyrus: in the terminal run
#                     $ uml
#   - Install SysML 1.4: Help --> Install Papyrus Additional Components --> SysML
#   - Install SysML 1.1: Help --> Install New Software... --> Update site: http://download.eclipse.org/modeling/mdt/papyrus/updates/releases/neon --> Papyrus SysML 1.1
if [ ! -f "${home}/.eclipse/papyrus/papyrus" ]; then
    echowarn "Please read the instructions in comments of .setup/install_dev_tools.sh for follow-up installation actions inside Papyrus!"
    runcmd "wget \"https://www.eclipse.org/downloads/download.php?file=/modeling/mdt/papyrus/rcp/neon/2.0.2/papyrus-neon-2.0.2-linux64.tar.gz&r=1\" -O ${home}/Downloads/papyrus.tar.gz"
    runcmd "mkdir -p ${home}/.eclipse/papyrus"
    runcmd "tar zxf ${home}/Downloads/papyrus.tar.gz --strip 1 -C ${home}/.eclipse/papyrus"
    runcmd "eval chown -R ${SUDO_USER:-$USER}:${SUDO_USER:-$USER} ${home}/.eclipse/papyrus"
    runcmd "rm -f ${home}/Downloads/papyrus.tar.gz"
fi


echo_prefix="$echo_prefix_temp"
