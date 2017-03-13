# ~/.bash_alises: my aliases for bash

# Enable using another alias after sudo
alias sudo='sudo '

# make sure the rm command, whatever it is, asks whether you really
# want to delete. Can save from accidentally deleting important
# files/folders.
alias rm='rm -i'

# Emacs
alias emacsclient="emacsclient -e '(my-start-emacs)' " # crucial part of layout persistence across sessions! (must come before below functions!!) (fixed, no longer needed I think...)
emacsserverstart() {
    local name="$1"
    if [ ! "$name" ]; then
	echo "Usage: emacsserverstart <SERVER_NAME>"
	return 1
    else
	emacs --eval "(setq server-name \"${name}\")" --daemon
    fi
}
emacsserverkill() {
    local name="$1"
    if [ ! "$name" ]; then
	echo "Usage: emacsserverkill <SERVER_NAME>"
	return 1
    else
	emacsclient -s "$name" -e '(kill-emacs)'
    fi
}
emacsserverconnect() {
    local name="$1"
    local windowtype="$2"
    local options="${*:3}"
    if [ ! "$name" ]; then
	echo "Usage: emacsserverconnect <SERVER_NAME> [w] [OPTIONS]"
	echo
	echo "Regular emacsclient OPTIONS are accepted"
	echo "Pass w option to launch as GUI"
	return 1
    else
	if [ "$windowtype" == "w" ]; then
	    local nwoption="-c"
	else
	    local nwoption="-t"
	fi
	if [ "$#" -gt 2 ]; then
	    emacsclient $nwoption -s "$name" "$options"
	else
	    emacsclient $nwoption -s "$name"
	fi
    fi
}
emacsserverlist() {
    local serverdir="${TMPDIR:-/tmp}/emacs${UID}"
    local -a servers
    for file in ${serverdir}/*; do
	if [[ -S ${file} ]]; then
	    servers+=("${file##*/}")  
	fi
    done
    echo "${servers[@]}"
}
emacsserver() {
    local options="${*:2}"
    emacsserver_usage() {
	echo "Usage: emacsserver [OPTIONS]"
	echo
	echo "The following OPTIONS are accepted:"
	echo "-l      List running servers"
	echo "-s      Start a server"
	echo "-k      Shutdown a server"
	echo "-c      Connect to a server"
	echo
    }

    local OPTIND
    while getopts "::lskc" option
    do
	case $option in
	    l)
		emacsserverlist
		return 0
		;;
	    s)
		emacsserverstart $options
		return 0
		;;
	    k)
		emacsserverkill $options
		return 0
		;;
	    c)
		emacsserverconnect $options
		return 0
		;;
	    *)
		emacsserver_usage
		return 1
		;;
	esac
    done
    emacsserver_usage
    return 1
}
alias em='emacs -nw '
alias ems='emacsserver '

# JetBrains products
alias clion='clion.sh & &>/dev/null'
alias pycharm='pycharm.sh & &>/dev/null'
alias idea='idea.sh & &>/dev/null'

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

# Fix typos in previous bash command
eval "$(thefuck --alias)"

# Screen
killscreens () {
    screen -ls | grep Detached | cut -d. -f1 | awk '{print $1}' | xargs kill
}

# Jupyter
alias venv_jupnb=". ~/.python_venv/jupnb/bin/activate && echo Use \'$ deactivate\' to quit the Jupyter notebook virtualenv"
