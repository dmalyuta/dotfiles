#!/bin/bash
# ------------------------------------------------------------------------------
#
# Installation of tools for engineering and, in particular, software/algorithms
# development.
#
# ------------------------------------------------------------------------------

echo_prefix_temp="$echo_prefix"
echo_prefix="[dev tools setup] "

# Latest version of Git
git_version="$(git --version | cut -d ' ' -f 3 | cut -d '.' -f 1)"
if [ "$git_version" -lt 2 ]; then
    # Need to upgrade Git
    apt_get_install_pkg python-software-properties
    runcmd "add-apt-repository ppa:git-core/ppa -y"
    runcmd "apt-get update"
    apt_get_install_pkg git
fi

# Build tools like gcc, g++
apt_get_install_pkg build-essential

# Java 8
java_version=$(java -version 2>&1 | grep "java version")
if ! echo "$java_version" | grep "1.8" &>/dev/null; then # TODO change to a check whether version is *at least* 1.8 (e.g. 1.9 is OK, don't do anything then)
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

# Meld Diff Viewer
# It is a GUI analog to the standard diff install diffutils and patch install patch command line tools
apt_get_install_pkg meld

# ghex
# Hex editor
apt_get_install_pkg ghex

# Sunflower Twin-panel file manager
if program_not_installed "sunflower"; then
    runcmd "wget http://sunflower-fm.org/pub/sunflower-0.3.61-1.all.deb -O /tmp/sunflower.deb"
    runcmd_noexit "dpkg -i /tmp/sunflower.deb" nonull
    runcmd "apt-get --assume-yes install -f" nonull
fi

# tmux (version 2.3)
# Terminal multiplexer
if [ "$(tmux -V)" != "tmux 2.3" ]; then
    apt_get_install_pkg libevent-dev
    apt_get_install_pkg libncurses5-dev
    wget_targz_install "tmux-2.3" "https://github.com/tmux/tmux/releases/download/2.3/tmux-2.3.tar.gz"
fi

## rr
# Debugging tool (recording & deterministic debugging)
if program_not_installed "rr"; then
    runcmd "wget https://github.com/mozilla/rr/releases/download/4.5.0/rr-4.5.0-Linux-$(uname -m).deb -O /tmp/rr.deb"
    runcmd "dpkg -i /tmp/rr.deb" nonull
fi

# Upgrade GDB to 7.12.1 (latest stable version)
# Includes many fixes, of which most important for Emacs is the `set max-completions`
# (see https://sourceware.org/git/gitweb.cgi?p=binutils-gdb.git;a=blobdiff;f=gdb/NEWS;h=f19577a3a6d0ea9ff1015255eafbd965580afa2d;hp=cba21b6645dd09e83943b71d42ad4c3d3c00cad4;hb=ef0b411a110cd2602cb89c3fb237baf8beb28545;hpb=e11c72c7e4879894b9711b5c0b8247c20c6050f6 and http://stackoverflow.com/questions/20933946/gdb-freezing-when-print-variables-with-tab-completion)
# Description: Sets the maximum number of candidates to be considered
# during completion. The default value is 200. This limit allows GDB
# to avoid generating large completion lists, the computation of which
# can cause the debugger to become temporarily unresponsive.
gdbVersion="$(/bin/echo $(gdb --version) | cut -d ' ' -f 4)"
if [ "$gdbVersion" != "7.12.1" ]; then
    # TODO make check only that $gdbVersion < 7.12.1 (so as not to downgrade if in the future user has a more recent version installed)
    wget_targz_install "gdb-7.12.1" "http://ftp.gnu.org/gnu/gdb/gdb-7.12.1.tar.gz"
fi

# GCC 7.1
# C/C++ compiler
gccVersion="$(/bin/echo $(gcc --version) | cut -d ' ' -f 4)"
if [ "$gccVersion" != "7.1.0" ]; then
	runcmd "add-apt-repository ppa:ubuntu-toolchain-r/test -y"
    runcmd "apt-get update"
    apt_get_install_pkg gcc-7
    apt_get_install_pkg g++-7
    runcmd "sudo update-alternatives --install /usr/bin/gcc gcc /usr/bin/gcc-7 60 --slave /usr/bin/g++ g++ /usr/bin/g++-7"
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
    runcmd "wget https://bitbucket.org/rhiokim/haroopad-download/downloads/haroopad-v0.13.1-x64.deb -O /tmp/haroopad.deb"
    runcmd_noexit "dpkg -i /tmp/haroopad.deb" nonull
    runcmd "apt-get --assume-yes install -f" nonull
fi

# Install SmartGit
if ! dpkg -l | grep -E '^ii' | grep smartgit &>/dev/null; then
    runcmd "wget http://www.syntevo.com/static/smart/download/smartgit/smartgit-17_0_3.deb -O /tmp/smartgit.deb"
    runcmd_noexit "dpkg -i /tmp/smartgit.deb" nonull
    runcmd "apt-get --assume-yes install -f"
fi

# Install Jetbrains Toolbox App (projects & programs manager)
if [ ! -d "${home}/.jetbrains/toolbox" ]; then
    runcmd "wget https://download.jetbrains.com/toolbox/jetbrains-toolbox-1.2.2314.tar.gz -O /tmp/toolbox.tar.gz"
    runcmd "rm -rf ${home}/.jetbrains/toolbox"
    runcmd "mkdir -p ${home}/.jetbrains/toolbox"
    runcmd "tar zxf /tmp/toolbox.tar.gz --strip 1 -C ${home}/.jetbrains/toolbox"
    runcmd "eval chown -R ${SUDO_USER:-$USER}:${SUDO_USER:-$USER} ${home}/.jetbrains/toolbox"
fi

# Install Jetbrains CLion (C/C++)
if [ ! -d "${home}/.jetbrains/clion" ]; then
    runcmd "wget https://download.jetbrains.com/cpp/CLion-2017.1.tar.gz -O /tmp/clion.tar.gz"
    runcmd "rm -rf ${home}/.jetbrains/clion"
    runcmd "mkdir -p ${home}/.jetbrains/clion"
    runcmd "tar zxf /tmp/clion.tar.gz --strip 1 -C ${home}/.jetbrains/clion"
    runcmd "eval chown -R ${SUDO_USER:-$USER}:${SUDO_USER:-$USER} ${home}/.jetbrains/clion"
fi

# Install Jetbrains PyCharm (Python)
if [ ! -d "${home}/.jetbrains/pycharm" ]; then
    runcmd "wget https://download.jetbrains.com/python/pycharm-professional-2017.1.1.tar.gz -O /tmp/pycharm.tar.gz"
    runcmd "rm -rf ${home}/.jetbrains/pycharm"
    runcmd "mkdir -p ${home}/.jetbrains/pycharm"
    runcmd "tar zxf /tmp/pycharm.tar.gz --strip 1 -C ${home}/.jetbrains/pycharm"
    runcmd "eval chown -R ${SUDO_USER:-$USER}:${SUDO_USER:-$USER} ${home}/.jetbrains/pycharm"
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
    runcmd "wget https://download1.rstudio.org/rstudio-1.0.136-amd64.deb -P /tmp/"
    runcmd_noexit "dpkg -i /tmp/rstudio-1.0.136-amd64.deb"
    runcmd "apt-get --assume-yes install -f"
fi

##### NB: below, Eclipse installations are essentially of Eclipse [VERSION] Platform Runtime Binary, which is the barebones minimalist Eclipse version (without any junk plugins preinstalled)

# Install Eclipse for C/C++, XML Web
#
# Follow up this installation with the following actions
#   - Launch Eclipse: $ eclipse_common
#   - Install CDT (C/C++): Help --> Install New Software... --> Update site: http://download.eclipse.org/tools/cdt/releases/9.2 --> CDT Main Features, CDT Optional Features
#   - Optionally install XML editing (Oxygen XML plugin): Help --> Install New Software... --> Update site: http://www.oxygenxml.com/InstData/Editor/Eclipse/site.xml --> oXygen XML Editor for Eclipse 3.6 -> 3.8, 4.2 -> 4.6
#   - Install Remote System Explorer: Help --> Install New Software... --> Update site: http://download.eclipse.org/releases/neon --> Search for "remote" --> General Purpose Tools/Remote System Explorer End-User Runtime
#   - Install integration with the terminal (EasyShell): Help --> Install New Software... --> Update site: http://anb0s.github.io/EasyShell --> EasyShell 2.0.x, PluginBox
if [ ! -f "${home}/.eclipse/eclipse_common/eclipse" ]; then
    echowarn "Please read the instructions in comments of .setup/install_dev_tools.sh for follow-up installation actions inside Eclipse!"
    runcmd "wget https://www.eclipse.org/downloads/download.php?file=/eclipse/downloads/drops4/R-4.6.3-201703010400/eclipse-platform-4.6.3-linux-gtk-x86_64.tar.gz&r=1 -O /tmp/eclipse_common.tar.gz"
    runcmd "mkdir -p ${home}/.eclipse/eclipse_common"
    runcmd "tar zxf /tmp/eclipse_common.tar.gz --strip 1 -C ${home}/.eclipse/eclipse_common"
    runcmd "eval chown -R ${SUDO_USER:-$USER}:${SUDO_USER:-$USER} ${home}/.eclipse"
fi

# Install Eclipse of UML/SysML
#
# TODO once Eclipse Oxygen is released, then might make sense to merge this and the above Eclipse installation for C/C++, Java, XML, etc.
#
# Follow up this installation with the following actions
#   - Install Web dev tools (for Papyrus CSS file editing): Help --> Install New Software... --> Update site: http://download.eclipse.org/releases/oxygen  --> Web, XML, Java EE and OSGi Enterprise Development/Eclipse Web Developer Tools
#   - Install Papyrus (UML/SysML): Help --> Install New Software... --> Update site: http://download.eclipse.org/modeling/mdt/papyrus/updates/nightly/oxygen --> Papyrus, Papyrus SysML 1.1
#       - Install SysML 1.4 [NOT YET POSSIBLE FOR OXYGEN]: Help --> Install Papyrus Additional Components --> SysML
if [ ! -f "${home}/.eclipse/eclipse_mbse/eclipse" ]; then
    echowarn "Please read the instructions in comments of .setup/install_dev_tools.sh for follow-up installation actions inside Eclipse!"
    runcmd "wget https://www.eclipse.org/downloads/download.php?file=/eclipse/downloads/drops4/S-4.7M6-201703082000/eclipse-platform-4.7M6-linux-gtk-x86_64.tar.gz&r=1 -O /tmp/eclipse_mbse.tar.gz"
    runcmd "mkdir -p ${home}/.eclipse/eclipse_mbse"
    runcmd "tar zxf /tmp/eclipse_mbse.tar.gz --strip 1 -C ${home}/.eclipse/eclipse_mbse"
    runcmd "eval chown -R ${SUDO_USER:-$USER}:${SUDO_USER:-$USER} ${home}/.eclipse"
fi

# Install Eclipse for LaTeX, Markdown
# 
# Follow up this installation with the following actions
#   - Launch Eclipse: $ eclipse_latex
#   - Install LaTeX editing (TeXlipse): Help --> Install New Software... --> Update site: http://texlipse.sourceforge.net --> Pdf4Eclipse, TeXlipse
#   - Install Markdown editor: Help --> Install New Software... --> Update site: http://www.certiv.net/updates --> Certiv Tools/FluentMark Editor
if [ ! -f "${home}/.eclipse/eclipse_latex/eclipse" ]; then
    echowarn "Please read the instructions in comments of .setup/install_dev_tools.sh for follow-up installation actions inside Eclipse!"
    runcmd "wget http://www.eclipse.org/downloads/download.php?file=/eclipse/downloads/drops4/R-4.5.2-201602121500/eclipse-platform-4.5.2-linux-gtk-x86_64.tar.gz&r=1 -O /tmp/eclipse_latex.tar.gz"
    runcmd "mkdir -p ${home}/.eclipse/eclipse_latex"
    runcmd "tar zxf /tmp/eclipse_latex.tar.gz --strip 1 -C ${home}/.eclipse/eclipse_latex"
    runcmd "eval chown -R ${SUDO_USER:-$USER}:${SUDO_USER:-$USER} ${home}/.eclipse"
fi


echo_prefix="$echo_prefix_temp"
