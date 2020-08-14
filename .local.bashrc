# ~/.local.bashrc: sourced from .bashrc in order to separate default
# .bashrc file from personal customizations.

# Terminator title settings with set_title <TITLE>
set_title() {
    printf "\e]2;$*\a";
}

# ROS setup
if [ -f "/opt/ros/indigo/setup.bash" ]; then
    . /opt/ros/indigo/setup.bash
fi
#if [ -f ~/catkin_ws/devel/setup.bash ]; then
#    . ~/catkin_ws/devel/setup.bash
#fi
# in ~/.local.bashrc optionally add the following lines for ROS network setup:
# export ROS_HOSTNAME=<local_IP_address (AAA.BBB.C.DDD)>
# export ROS_IP=<local_IP_address (AAA.BBB.C.DDD)>
# export ROS_MASTER_URI=<master_URI (http://EEE.FFF.H.III:JKLMN)>

# Private bashrc file
if [ -f ~/.local.bashrc.private ]; then
    . ~/.local.bashrc.private
fi

# checksum check
checksum_check() {
    local file="$1"
    local checksum="$2"

    if [ "$(md5sum "$file")" == "$checksum" ]; then
	echo "OK"
    else
	echo "FAIL"
    fi
}

# Go language
if [[ -d /usr/local/go ]]; then
    export GOROOT=/usr/local/go
    export GOPATH=$HOME/gopath
    export PATH=$GOPATH:$GOPATH/bin:$GOROOT/bin:$PATH
fi

##################################### BASH PROMPT
parse_git_branch() {
    # Display git branch
    git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'
}
parse_return_code() {
    # Display return code of last command, if non-zero (i.e. last
    # command failed)
    local last_return_code=$?
    if [ $last_return_code -ne 0 ]; then
	echo " [$last_return_code]"
    else
	echo ""
    fi
}
parse_ssh() {
    # Print if using computer over ssh
    if [ -n "$SSH_CLIENT" ] || [ -n "$SSH_TTY" ]; then
	echo "(ssh) "
	# many other tests omitted
    else
	case $(ps -o comm= -p $PPID) in
	    sshd|*/sshd) echo "(ssh) ";;
	esac
    fi    
}
if [ -e ~/.bin/colorizer ]; then
    . ~/.bin/colorizer/Library/colorizer.sh
    tmp=$(colorize -p ">>> [<purple>\D{%b %d} \A</purple>] <orange>$(parse_ssh)</orange><white></white>\u <green>\w</green><red>\$(parse_return_code)</red><yellow>\$(parse_git_branch)</yellow>")
    export PS1="${tmp}\n$ "
fi

# Use emacsclient for Git
export GIT_EDITOR="`which emacsclient` -t -s git"

# Synaptics touchpad config
# if [ -f ~/.touchpad_config.sh ]; then
#     . ~/.touchpad_config.sh
# fi
