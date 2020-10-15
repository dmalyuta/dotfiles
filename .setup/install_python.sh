#!/bin/bash
# ----------------------------------------------------------------------
#
# Installation of tools for better Python programming experience.
#
# ----------------------------------------------------------------------

echo_prefix_temp="$echo_prefix"
echo_prefix="[python tools setup] "

apt_get_install_pkg python
# install fresh pip
#apt_get_install_pkg python-dev
#apt_get_install_pkg python-pip
#apt_get_install_pkg python3-dev
apt_get_install_pkg python3-pip
#runcmd "sudo -H python -m pip install --upgrade --force pip"

########################
# Anaconda
########################

if function_not_defined "conda"; then
    runcmd "wget -4 https://repo.anaconda.com/archive/Anaconda3-2019.03-Linux-x86_64.sh -O /tmp/anaconda.sh"
    runcmd_noexit "chmod +x /tmp/anaconda.sh" nonull
    runcmd_noexit "/tmp/anaconda.sh" nonull
fi

########################
# Python 3.8.5
########################

PYENV_NAME=py385
CONDA_CONFIG_PATH=$(conda info --base)/etc/profile.d/conda.sh

if ! (conda info --envs | grep $PYENV_NAME > /dev/null 2>&1); then
    (runcmd "source $CONDA_CONFIG_PATH" &&
	 runcmd "conda create -y -n $PYENV_NAME python=3.8.5" nonull &&
	 runcmd "conda activate $PYENV_NAME" &&
	 runcmd "conda install -y ipython" nonull)

    # Make it the default virtualenv on bash startup
    runcmd "eval builtin echo \"\" >> ${home}/.bashrc" nonull
    runcmd "eval builtin echo \"conda activate py385\" >> ${home}/.bashrc" nonull

    # Install some modules
    runcmd "pip install pandas"
    runcmd "pip install pytest"
    runcmd "pip install black"
fi

########################
# Setup reloading in Ipython
########################

# Make sure you are running IPython 5.7.0, because buggy for later versions
# ``$ pip install -U ipython==5.7.0``
#
# Fix autoreload problem (answer by DmitrySemenov at
# https://tinyurl.com/ipython-autoreload):
#
# I found a better solution that needs no emacs config: simply do
#
# $ ipython profile create
#
# that should create ipython profile in
# $HOME/.ipython/profile_default/ipython_config.py
# then put the following inside
# ```
# c = get_config()
# c.TerminalInteractiveShell.editor = 'emacsclient'
# c.InteractiveShellApp.extensions = [
#      'autoreload'
# ]
#
# c.InteractiveShellApp.exec_lines = []
# c.InteractiveShellApp.exec_lines.append('%load_ext autoreload')
# c.InteractiveShellApp.exec_lines.append('%autoreload 2')

echo_prefix="$echo_prefix_temp"
