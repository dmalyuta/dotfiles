#!/bin/bash
# ----------------------------------------------------------------------
#
# Installation of tools for better Python programming experience.
#
# ----------------------------------------------------------------------

echo_prefix_temp="$echo_prefix"
echo_prefix="[python tools setup] "

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
fi

echo_prefix="$echo_prefix_temp"
