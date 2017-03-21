# ~/.bash_alises: my aliases for bash

# Enable using another alias after sudo
alias sudo='sudo '

# make sure the rm command, whatever it is, asks whether you really
# want to delete. Can save from accidentally deleting important
# files/folders.
alias rm='rm -i'

# Clipboard
alias copy='xargs echo -n | xclip -selection clipboard'

# MATLAB terminal
alias matlabterminal='matlab -nodesktop -nosplash'

# Directory making
now() { date "+%d%m%YT%H%M%S"; } # shortcut for timestamps

# Grep contents of files in <dir> for <pattern>
alias greptext='grep -rnw'

# ROS stuff
alias catkin_make_debug='catkin_make -DCMAKE_BUILD_TYPE=Debug'
alias catkin_make_release='catkin_make -DCMAKE_BUILD_TYPE=Release'
alias catkin_make_release_debug='catkin_make -DCMAKE_BUILD_TYPE=RelWithDebInfo'
compile_commands_json() { echo "-DCMAKE_EXPORT_COMPILE_COMMANDS=ON"; } # generate compile_commands.json for use with Emacs irony mode via 'M-x irony-cdb-json-add-compile-commands-path' (syntax check, autocompletion)

# Screen
killscreens () {
    screen -ls | grep Detached | cut -d. -f1 | awk '{print $1}' | xargs kill
}

# Jupyter
alias venv_jupnb=". ~/.python_venv/jupnb/bin/activate && echo Use \'$ deactivate\' to quit the Jupyter notebook virtualenv"

# JetBrains products
alias clion='~/.jetbrains/clion/bin/clion.sh & 2&>/dev/null && disown'
