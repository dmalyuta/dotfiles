#!/bin/bash
# shellcheck disable=SC1090
#
# Python programming tools.
#
# Author: Danylo Malyuta, 2020.

sudo apt-get -y install python3 python3-pip
sudo update-alternatives --install /usr/bin/python python /usr/bin/python3 100

# ..:: Anaconda ::..

if not_installed conda; then
    wget -4 https://repo.anaconda.com/archive/Anaconda3-2021.11-Linux-x86_64.sh \
	 -O /tmp/anaconda.sh
    chmod +x /tmp/anaconda.sh
    /tmp/anaconda.sh

    # Update conda
    conda install anaconda
    conda update -n base -c defaults conda
    
    # Make changes take effect
    source ~/.bashrc
fi

# ..:: Default Python environment ::..

PYENV_NAME=py3104
CONDA_PATH=$(conda info --base)/etc/profile.d/conda.sh

if ! (conda info --envs | grep -q $PYENV_NAME); then
    source "$CONDA_PATH"
    conda create -y -n $PYENV_NAME python=3.10.4
    conda activate $PYENV_NAME
    conda install -y ipython

    # Make it the default virtualenv on bash startup
    cat << EOF >> ~/.bashrc
conda activate $PYENV_NAME
EOF

    # Install some Python modules
    pip install jedi
    pip install flake8 pdbpp
    pip install scipy numpy nptyping
    pip install pandas pytest black pyfzf
    pip install virtualenv

    # Add virtualenv to Jupyter
    # https://gist.github.com/swedishmike/902fb27d627313c31a95e31c44e302ac
    pip install --user ipykernel
    python -m ipykernel install --user --name=py3104
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
