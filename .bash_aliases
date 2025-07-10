#.bash_aliases

################################################################################
# Aliases.
################################################################################

alias refresh='source ~/.bashrc'
alias which_venv='pip -V'
alias ll='ls -l'
alias lsd='ll -d */'
alias peek='tree -L 1'
alias copy='DISPLAY=$DISPLAY xclip -sel clip'
alias gitroot='cd $(git rev-parse --show-toplevel)'
alias sleep='systemctl suspend'

################################################################################
# Process management.
################################################################################

killgrep() {
    ps -ef | grep -E "$1" | grep -v grep | awk '{print $2}' | xargs kill
}

psgrep() {
    ps aux | grep $1
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
        emacsclient -s $server_name -e "(kill-emacs)"
        if [ $? -eq 0 ]; then
            echo "✅ Stopped $server_name"
        else
            echo "❌ Failed to stop $server_name"
        fi
    done
}

emacs_restart_all() {
    emacs_stop
    emacs_start
}

emacs_list_servers() {
    #ls -1 /run/user/1000/emacs
    ps aux | grep -E "emacs.*--daemon=.*" |
        awk '/emacs / { match($0, /--daemon=([^ \(]+)/, m); if (m[1] != "") print m[1] }'
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

    window_count=$(emacsclient -s $name -e "(length (frame-list))")

    if [ -z "$file" ]; then
        if [ "$window_count" = "1" ]; then
            nohup emacsclient -c -s "$name" -e "$EMACS_DEFAULT_SIZE" >/dev/null 2>&1 &
            disown
        else
            nohup emacsclient -s "$name" >/dev/null 2>&1 &
            disown
        fi
    else
        if [ "$window_count" = "1" ]; then
            nohup emacsclient -c -s "$name" -e "$EMACS_DEFAULT_SIZE" "(find-file \"$file\")" >/dev/null 2>&1 &
            disown
        else
            nohup emacsclient -s "$name" -e "(find-file \"$file\")" >/dev/null 2>&1 &
            disown
        fi
    fi
}
