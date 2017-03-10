# ~/.bash_alises: my aliases for bash

# Enable using another alias after sudo
alias sudo='sudo '

# make sure the rm command, whatever it is, asks whether you really
# want to delete. Can save from accidentally deleting important
# files/folders.
alias rm='rm -i'

# Emacs
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
    local options="${*:2}"
    if [ ! "$name" ]; then
	echo "Usage: emacsserverconnect <SERVER_NAME> [OPTIONS]"
	return 1
    else
	if [ "$#" -ne 1 ]; then
	    emacsclient -s "$name" -nw "$options"
	else
	    emacsclient -s "$name" -nw
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
		emacsserverstart "$options"
		return 0
		;;
	    k)
		emacsserverkill "$options"
		return 0
		;;
	    c)
		emacsserverconnect "$options"
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
alias em='emacs -nw'
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
