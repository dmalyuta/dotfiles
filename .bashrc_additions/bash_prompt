
# display git branch and a failed command's error code in the bash
# prompt
parse_git_branch() {
    git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'
}
parse_return_code() {
    local last_return_code=$?
    if [ $last_return_code -ne 0 ]; then
	echo " [$last_return_code]"
    else
	echo ""
    fi
}
parse_ssh() {
    if [ -n "$SSH_CLIENT" ] || [ -n "$SSH_TTY" ]; then
	echo "(ssh) "
	# many other tests omitted
    else
	case $(ps -o comm= -p $PPID) in
	    sshd|*/sshd) echo "(ssh) ";;
	esac
    fi    
}
export PS1="$(parse_ssh)\u \[\033[32m\]\w\[\033[1;31m\]\$(parse_return_code)\[\033[0;33m\]\$(parse_git_branch)\[\033[00m\] $ "