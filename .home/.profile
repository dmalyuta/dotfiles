# ~/.bash_profile: used for login shells, run at login either of the
# local computer or when starting an ssh session. Put non-bash related
# stuff here like setting the PATH/one-time configuration of the
# working environment.

# source .bashrc
# see http://stackoverflow.com/questions/820517/bashrc-at-ssh-login
if [ -f ~/.bashrc ]; then
    . ~/.bashrc
fi

# add .bin directory to path, which includes custom
# user scripts
if [ -d ~/.bin ]; then
    PATH="${HOME}/.bin:$PATH"
fi

# Start Emacs servers whenever any are not running.
emacs_start
