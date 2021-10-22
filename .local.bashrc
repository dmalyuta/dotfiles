# ~/.local.bashrc: sourced from .bashrc in order to separate default
# .bashrc file from personal customizations.

# Terminator title settings with set_title <TITLE>
set_title() {
    printf "\e]2;$*\a";
}

# Private bashrc file
if [ -f ~/.local.bashrc.private ]; then
    source ~/.local.bashrc.private
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

# ..:: Bash prompt ::..

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

if [ -f ~/.bin/colorizer/Library/colorizer.sh ]; then
    source ~/.bin/colorizer/Library/colorizer.sh

    tmp=$(colorize -p ">>> [<purple>\D{%b %d} \A</purple>] " \
		   "<orange>$(parse_ssh)</orange><white></white>\u " \
		   "<green>\w</green><red>\$(parse_return_code)</red><yellow>" \
		   "\$(parse_git_branch)</yellow>")

    export PS1="${tmp}\n$ "
fi

# Use emacs as terminal editor
# See the different between VISUAL vs. EDITOR:
#  https://unix.stackexchange.com/questions/4859/visual-vs-editor-what-s-the-difference
export EDITOR="emacs -nw"

# Use VSCode as visual editor
export VISUAL="code --wait"

# Synaptics touchpad config
if [ -f ~/.input_config.sh ]; then
    source ~/.input_config.sh
fi

# Vterm configuration
# https://github.com/akermu/emacs-libvterm/blob/master/README.md

# Shell-side config
vterm_printf(){
    if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ] ); then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}

# Terminal output clearing
if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
    function clear(){
        vterm_printf "51;Evterm-clear-scrollback";
        tput clear;
    }
fi

# Directory tracking
vterm_prompt_end(){
    if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
	vterm_printf "51;A$(whoami)@$(hostname):$(pwd)"
    fi
}
PS1=$PS1'\[$(vterm_prompt_end)\]'

# Make logseq executable
export PATH="$PATH:$HOME/.local/bin/"
