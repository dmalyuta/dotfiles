#!/bin/bash
# shellcheck disable=SC1090
#
# Python programming tools.
#
# Author: Danylo Malyuta, 2020.

sudo apt-get -y install python python3-pip

# ..:: Anaconda ::..

if not_installed conda; then
    wget -4 https://repo.anaconda.com/archive/Anaconda3-2021.05-Linux-x86_64.sh \
	 -O /tmp/anaconda.sh
    chmod +x /tmp/anaconda.sh
    /tmp/anaconda.sh

    # Update conda
    conda install anaconda
    conda update -n base -c defaults conda
fi

# ..:: Default Python environment ::..

PYENV_NAME=py397
CONDA_PATH=$(conda info --base)/etc/profile.d/conda.sh

if ! (conda info --envs | grep -q $PYENV_NAME); then
    source "$CONDA_PATH"
    conda create -y -n $PYENV_NAME python=3.9.7
    conda activate $PYENV_NAME
    conda install -y ipython

    # Make it the default virtualenv on bash startup
    echo "" >> ~/.bashrc
    echo "conda activate py397" >> ~/.bashrc

    # Install some Python modules
    pip install jedi
    pip install flake8 pdbpp
    pip install scipy numpy nptyping
    pip install pandas pytest black pyfzf
    pip install vtk==9.0.3
    pip install mayavi
    pip install PySide2
    pip install virtualenv

    # Add virtualenv to Jupyter
    # https://gist.github.com/swedishmike/902fb27d627313c31a95e31c44e302ac
    pip install --user ipykernel
    python -m ipykernel install --user --name=py397
fi

# ..:: Other tools ::..

# Profiling
sudo apt-get -y install pyprof2calltree

# Add the virtualenv path to the PATH
if ! echo "$PATH" | grep -q virtualenv; then
    echo export PATH="$PATH":"$(command -v virtualenv)" >> ~/.bashrc
fi

# ..:: Jupyter notebook ::..

sudo apt-get -y install jupyter \
     jupyter-notebook
pip install jupyterlab

# diffing and merging of Jupyter Notebooks
pip install nbdime

# ..:: Git ::..

# README file local preview
pip install grip
