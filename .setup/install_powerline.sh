#!/bin/bash
# ----------------------------------------------------------------------
#
# powerline installation.
#
# Powerline is a statusline plugin for vim, and provides statuslines
# and prompts for several other applications, including zsh, bash,
# tmux, IPython, Awesome and Qtile.
#
# To learn more about powerline, visit:
# https://github.com/powerline/powerline
#
# ----------------------------------------------------------------------

echo_prefix_temp="$echo_prefix"
echo_prefix="[powerline setup] "

if program_not_installed "powerline"; then
    # install powerline
    # following instructions: https://powerline.readthedocs.io/en/latest/installation.html#pip-installation

    apt_get_install_pkg python3
    apt_get_install_pkg python-pip
    apt_get_install_pkg fontconfig
    runcmd "sudo -H pip install --upgrade pip"
    runcmd "sudo -H pip install setuptools"
    runcmd "sudo -H pip install powerline-status"

    # install powerline fonts
    # following instructions: https://powerline.readthedocs.io/en/latest/installation/linux.html

    runcmd "wget https://github.com/powerline/powerline/raw/develop/font/PowerlineSymbols.otf"
    runcmd "wget https://github.com/powerline/powerline/raw/develop/font/10-powerline-symbols.conf"

    font_dir="${home}/.fonts"
    makefolder "$font_dir"
    move_foo "PowerlineSymbols.otf" "$realdir" "$font_dir"
    runcmd "fc-cache -vf ${font_dir}"

    fontconfig_dir="${home}/.config/fontconfig/conf.d"
    makefolder "$fontconfig_dir"
    move_foo "10-powerline-symbols.conf" "$realdir" "$fontconfig_dir"

    # enable powerline for bash
    # following instructions: https://powerline.readthedocs.io/en/latest/usage/shell-prompts.html#bash-prompt
    repository_root=$(sudo -H pip show powerline-status | grep Location | cut -d " " -f 2)
    msg=". ${repository_root}/powerline/bindings/bash/powerline.sh"
    if ! $(cat "${home}/.bashrc" | grep "export TERMS="); then
	# line not already in ~/.bashrc, so append it
	runcmd "builtin echo \"${msg}\" >> ${home}/.bashrc" nonull
    fi

    # configure powerline
    # following instructions: https://github.com/adidenko/powerline
    runcmd "git clone https://github.com/adidenko/powerline"
    move_foo "powerline" "$realdir" "${home}/.config"
fi

echo_prefix="$echo_prefix_temp"

exit 0
