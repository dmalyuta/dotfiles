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

# Emacs ansi-term directory tracking
# if [ "$TERM" = eterm-color ]; then
#     function eterm-set-cwd {
#         $@
#         echo -e "\033AnSiTc" $(pwd)
#     }
    
#     # set hostname, user, and cwd
#     function eterm-reset {
#         echo -e "\033AnSiTu" $(whoami)
#         echo -e "\033AnSiTc" $(pwd)
#         echo -e "\033AnSiTh" $(hostname)
#     }
    
#     for temp in cd pushd popd; do
#         alias $temp="eterm-set-cwd $temp"
#     done
    
#     # set hostname, user, and cwd now
#     eterm-reset
# fi

# if [ -f ~/.local.bashrc ]; then
#     . ~/.local.bashrc
# fi

export EDITOR=$(type -P emacs || type -P nano)
export VISUAL=$EDITOR
