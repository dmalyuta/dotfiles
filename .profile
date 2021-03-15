# ~/.bash_profile: used for login shells, run at login either of the
# local computer or when starting an ssh session. Put non-bash related
# stuff here like setting the PATH/one-time configuration of the
# working environment.

# source .bashrc
# see http://stackoverflow.com/questions/820517/bashrc-at-ssh-login
if [ -f ~/.bashrc ]; then
    . ~/.bashrc
fi

# personal additions to .bashrc
if [ -f ~/.local.bashrc ]; then
    . ~/.local.bashrc
fi

# add .bin directory to path, which includes custom
# user scripts
if [ -d ~/.bin ]; then
    PATH="${HOME}/.bin:$PATH"
fi

# Mouse cursor size
gsettings set org.gnome.desktop.interface cursor-size 22

# Disable the dock (use Rofi to view open windows list)
gnome-extensions disable ubuntu-dock@ubuntu.com
