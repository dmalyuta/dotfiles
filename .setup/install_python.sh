#!/bin/bash
# ----------------------------------------------------------------------
#
# Installation of tools for better Python programming experience.
#
# ----------------------------------------------------------------------

echo_prefix_temp="$echo_prefix"
echo_prefix="[python tools setup] "

########################
####### Jupyter notebook
#######################
# Instructions from:
# http://bikulov.org/blog/2015/11/07/install-jupyter-notebook-and-scientific-environment-in-ubuntu-14-dot-04-with-python-3/

### Install prerequisites

# install curl virtualenvwrapper
apt_get_install_pkg curl
apt_get_install_pkg virtualenvwrapper

# install as workaround for https://github.com/matplotlib/matplotlib/issues/3029/
apt_get_install_pkg pkg-config

# install python development packages and g++
apt_get_install_pkg python3-dev
apt_get_install_pkg g++

# install dependencies for scipy
apt_get_install_pkg libblas-dev
apt_get_install_pkg liblapack-dev
apt_get_install_pkg gfortran

# install some dependencies for matplotlib
apt_get_install_pkg libfreetype6-dev
apt_get_install_pkg libpng-dev
apt_get_install_pkg libjpeg8-dev


### Now create and activate virtualenv, install all the packages via pip

# create and activate virtual environment using mkvirtualenv wrapper (env name is jupnb)
runcmd "mkvirtualenv --no-setuptools --python /usr/bin/python3.4 jupnb"

# install fresh pip
runcmd "curl https://bootstrap.pypa.io/get-pip.py | python"

# install fresh setuptools
runcmd "pip install setuptools distribute"

# install numpy as it is dependecy for many others
runcmd "pip install numpy"

# install scientific packages (seaborn instead of matplotlib for pretty plots)
runcmd "pip install sympy scipy seaborn pandas jupyter"

# install scikit-learn separately, it depends on numpy and scipy
runcmd "pip install scikit-learn"

# deactivate venv
runcmd "deactivate"

# Configure notebook profile with SSL encryption. Use real ssl
# certificate instead of self-generated (if you have one). You can use
# jupyter notebook --generate-config command to create default config
# with comments about all the options, or use following snippet for
# minimal config generation:

runcmd "mkdir -p ~/.jupyter"
runcmd "cd ~/.jupyter"
runcmd "jupyter notebook --generate-config"
runcmd "cd -"


echo_prefix="$echo_prefix_temp"
