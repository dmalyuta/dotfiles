# check alias after sudo
# see 
alias sudo='sudo '

# make sure the rm command, whatever it is, asks whether you really
# want to delete. Can save from accidentally deleting important
# files/folders.
alias rm='rm -i'

# Emacs
alias em='emacs -nw'

# JetBrains products
alias clion='clion.sh & &>/dev/null'
alias pycharm='pycharm.sh & &>/dev/null'
alias idea='idea.sh & &>/dev/null'

# Clipboard
alias copy='xargs echo -n | xclip -selection clipboard'

# Directory making
now() { echo $(date "+%d%m%YT%H%M%S"); } # shortcut for timestamps

# ROS stuff
alias catkin_make_debug='catkin_make -DCMAKE_BUILD_TYPE=Debug'
alias catkin_make_release='catkin_make -DCMAKE_BUILD_TYPE=Release'
alias catkin_make_release_debug='catkin_make -DCMAKE_BUILD_TYPE=RelWithDebInfo'
cmake_compile_commands_json='-DCMAKE_EXPORT_COMPILE_COMMANDS=ON'
