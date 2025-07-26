#.bash_aliases

################################################################################
# Aliases.
################################################################################

alias refresh='source ~/.bashrc'
alias which_venv='pip -V'
alias ll='ls -al'
alias lsd='ll -d */'
alias peek='tree -L 1'
alias copy='DISPLAY=$DISPLAY xclip -sel clip'
alias gitroot='cd $(git rev-parse --show-toplevel)'
alias sleep='systemctl suspend'
alias dotfiles='cd ~/Documents/dotfiles && ll'

################################################################################
# Process management.
################################################################################

killgrep() {
    local input_regexp="$@"
    ps -ef | grep -E "$@" | grep -v grep | awk '{print $2}' | xargs kill
}

psgrep() {
    local input_regexp="$@"
    ps aux | grep -E "$input_regexp"
}

################################################################################
# Navigation.
################################################################################

cs() {
    cd $1
    ll
}

################################################################################
# Search.
################################################################################

find_file() {
    local FILENAME=$1
    shift
    find ~+ $@ -type f -name "$FILENAME" 2>&1 | grep -v 'Permission denied'
}

find_symlink() {
    local FILENAME=$1
    shift
    find ~+ $@ -type l -name "$FILENAME"
}

find_file_or_symlink() {
    echo "=== Files ==="
    find_file $@
    echo ""
    echo "=== Symlinks ==="
    find_symlink $@
}

find_folder() {
    local FILENAME=$1
    shift
    find ~+ $@ -type d -name "$FILENAME"
}

find_string() {
    if [ $# -eq 2 ]; then
        ag --follow --hidden -G "$1" "$2"
    else
        ag --follow --hidden "$1"
    fi
}

################################################################################
# Emacs.
################################################################################

# Create emacs server and connect to server
EMACS_DEFAULT_DAEMON="emacs-server"
EMACS_DEFAULT_SIZE="(set-frame-size (selected-frame) 100 40)"
EMACS_USE_NW__INTERNAL=false

emacs_start() {
    local logfile=/tmp/emacs_servers_init.log
    >$logfile
    pgrep -f "emacs.*git-server" >/dev/null
    if [ $? -ne 0 ]; then
        (
            nohup emacs --daemon=git-server &>>$logfile </dev/null &
            disown
        ) &>/dev/null
        echo "✅ Started git-server"
    fi
    pgrep -f "emacs.*${EMACS_DEFAULT_DAEMON}" >/dev/null
    if [ $? -ne 0 ]; then
        (
            nohup emacs --daemon=$EMACS_DEFAULT_DAEMON &>>$logfile </dev/null &
            disown
        ) &>/dev/null
        echo "✅ Started $EMACS_DEFAULT_DAEMON"
    fi
}

emacs_stop() {
    local grep_query=$1
    if [ -z "$grep_query" ]; then
        local emacs_servers=$(emacs_list_servers)
    else
        local emacs_servers=$(emacs_list_servers | grep -E "${grep_query}")
    fi
    for server_name in $emacs_servers; do
        echo $server_name
        local server_description="default server"
        if [ "$server_name" == "emacs--default-server" ]; then
            emacsclient -e "(kill-emacs)"
        else
            emacsclient -s $server_name -e "(kill-emacs)"
            server_description=$server_name
        fi
        if [ $? -eq 0 ]; then
            echo "✅ Stopped $server_description"
        else
            echo "❌ Failed to stop $server_description"
        fi
    done
}

emacs_restart_all() {
    emacs_stop
    emacs_start
}

emacs_list_servers() {
    ps aux | grep -E "emacs.*--daemon.*" | awk '/\semacs --daemon=?/ {
    match($0, /(--daemon=?)([^ \(]+)/, m);
    if (m[1] != "") {
        print m[2];
    }
    else
    {
        print "emacs--default-server"
    }
}'
}

emacs_connect_new() {
    name=$1
    file=$2
    if [ -z "$2" ]; then
        name="$EMACS_DEFAULT_DAEMON"
        file="$1"
    fi
    if [ -z "$file" ]; then
        nohup emacsclient -c -s "$name" -e "$EMACS_DEFAULT_SIZE" >/dev/null 2>&1 &
        disown
    else
        nohup emacsclient -c -s "$name" -e "$EMACS_DEFAULT_SIZE" "(find-file \"$file\")" >/dev/null 2>&1 &
        disown
    fi
}

emacs_connect() {
    name=$1
    file=$2

    if [ -z "$2" ]; then
        name="$EMACS_DEFAULT_DAEMON"
        file="$1"
    fi

    # Add the emacsclient command itself, optionally with -nw for TTY mode.
    local -a emacs_cmd=()
    emacs_cmd+=("emacsclient")

    # Specify TTY mode.
    is_gui=true
    if [[ $EMACS_USE_NW__INTERNAL == true ]]; then
        emacs_cmd+=("-nw")
        is_gui=false
    fi

    # Connect if no open windows.
    window_count=$(emacsclient -s $name -e "(length (frame-list))")
    new_window=false
    if [[ ($window_count == 1) && ($is_gui == true) ]]; then
        emacs_cmd+=("-c")
        new_window=true
    fi

    # Select server to connect to.
    emacs_cmd+=("-s" "$name")

    # If creating a new window, specify the size.
    local -a emacs_exec=()
    if [[ $new_window == true ]]; then
        emacs_exec+=("$EMACS_DEFAULT_SIZE")
    fi

    # If opening a new file, execute the find-file command in the Emacs
    # instance.
    if [[ -n $file ]]; then
        emacs_exec+=("(find-file \"$file\")")
    fi

    # Append exec statements to the overall command.
    if [[ ${#emacs_exec[@]} -gt 0 ]]; then
        emacs_cmd+=("-e" "${emacs_exec[@]}")
    fi

    # Evaluate the command.
    echo  Executing: "${emacs_cmd[@]}"
    if [[ $is_gui == true ]]; then
        nohup "${emacs_cmd[@]}" >/dev/null 2>&1 &
        disown
    else
        "${emacs_cmd[@]}"
    fi
}

emacs_connect_nw() {
    EMACS_USE_NW__INTERNAL=true
    emacs_connect $1 $2
    EMACS_USE_NW__INTERNAL=false
}
