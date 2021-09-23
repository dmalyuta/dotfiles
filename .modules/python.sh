#!/bin/bash
# shellcheck disable=SC1090
#
# Python programming tools.
#
# Author: Danylo Malyuta, 2020.

sudo apt-get -y install python python3-pip

# ..:: Anaconda ::..

if not_installed conda; then
    wget -4 https://repo.anaconda.com/archive/Anaconda3-2019.03-Linux-x86_64.sh \
	 -O /tmp/anaconda.sh
    chmod +x /tmp/anaconda.sh
    /tmp/anaconda.sh
fi

# ..:: Python 3.8.5 ::..

PYENV_NAME=py385
CONDA_PATH=$(conda info --base)/etc/profile.d/conda.sh

if ! (conda info --envs | grep -q $PYENV_NAME); then
    source "$CONDA_PATH"
    conda create -y -n $PYENV_NAME python=3.8.5
    conda activate $PYENV_NAME
    conda install -y ipython

    # Make it the default virtualenv on bash startup
    echo "" >> ~/.bashrc
    echo "conda activate py385" >> ~/.bashrc

    # Install some Python modules
    pip install pandas
    pip install pytest
    pip install black
fi

# ..:: Other tools ::..

sudo apt-get -y install pyprof2calltree

# ..:: Jupyter notebooks ::..

sudo apt-get -y install jupyter-notebook
