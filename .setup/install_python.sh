#!/bin/bash
# ----------------------------------------------------------------------
#
# Installation of tools for better Python programming experience.
#
# ----------------------------------------------------------------------

echo_prefix_temp="$echo_prefix"
echo_prefix="[python tools setup] "

# install fresh pip
apt_get_install_pkg python-dev
apt_get_install_pkg python-pip
apt_get_install_pkg python3-dev
apt_get_install_pkg python3-pip
runcmd "sudo -H python -m pip install --upgrade --force pip"
runcmd "sudo -H pip install setuptools==33.1.1"
# runcmd "eval curl https://bootstrap.pypa.io/get-pip.py | python"c

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

# install g++
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
# if [ ! -d "${home}/.python_venv/jupnb" ]; then
#     runcmd "sudo -u ${normal_user} /usr/local/bin/virtualenv --no-setuptools --python /usr/bin/python3.4 ${home}/.python_venv/jupnb"
# fi
# runcmd ". ${home}/.python_venv/jupnb/bin/activate"

# install fresh setuptools
runcmd "sudo -H pip install setuptools distribute"

# install numpy as it is dependecy for many others
runcmd "sudo -H pip install numpy"

# install scientific packages (seaborn instead of matplotlib for pretty plots)
runcmd "sudo -H pip install sympy scipy seaborn pandas jupyter"

# install scikit-learn separately, it depends on numpy and scipy
runcmd "sudo -H pip install scikit-learn"

(runcmd "sudo -u ${normal_user} mkdir -p ${home}/.jupyter" && \
	runcmd "cd ${home}/.jupyter" && \
	runcmd "eval chown -R ${normal_user}:${normal_user} \"${home}/.local/share/jupyter\"")
#runcmd "sudo -u ${normal_user} jupyter notebook --generate-config" nonull
#runcmd "eval chown -R ${normal_user}:${normal_user} \"${home}/.jupyter\""

##################
####### JupyterLab
##################

runcmd "sudo -H pip install jupyterlab"
runcmd "jupyter serverextension enable --py jupyterlab --sys-prefix"

#######################
####### Other utilities
#######################

# Pandoc (document conversion, e.g. for Jupyter Notebook ---> PDF)
if program_not_installed "pandoc"; then
    runcmd "wget https://github.com/jgm/pandoc/releases/download/1.19.2.1/pandoc-1.19.2.1-1-amd64.deb -O /tmp/pandoc.deb"
    runcmd "dpkg -i /tmp/pandoc.deb"
    runcmd "apt-get --assume-yes install -f"
fi

# wkhtmltopdf (converter HTML to PDF)
if program_not_installed "wkhtmltopdf"; then
    runcmd "wget https://downloads.wkhtmltopdf.org/0.12/0.12.4/wkhtmltox-0.12.4_linux-generic-amd64.tar.xz -O /tmp/wkhtmltox.tar.xz"
    runcmd "rm -rf /tmp/wkhtmltox"
    runcmd "tar xf /tmp/wkhtmltox.tar.xz -C /tmp"
    runcmd "mv /tmp/wkhtmltox/bin/* /usr/bin/"
    runcmd "mv /tmp/wkhtmltox/include/* /usr/include/"
    runcmd "mv /tmp/wkhtmltox/lib/* /usr/lib/"
    runcmd "mv /tmp/wkhtmltox/share/man/man1/* /usr/share/man/man1/"
fi

# Password keyring
runcmd "sudo -H pip install keyring"
 

echo_prefix="$echo_prefix_temp"
