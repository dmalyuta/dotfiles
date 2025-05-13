#.bash_aliases

################################################################################
# Aliases.
################################################################################

alias refresh='source ~/.bashrc'
alias which_venv='pip -V'
alias ll='ls -l'
alias lld='ll -d */'
alias peek='tree -L 1'
alias copy='DISPLAY=:0 xclip -sel clip'
alias gitroot='cd $(git rev-parse --show-toplevel)'

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
        ag --hidden -G "$1" "$2"
    else
        ag --hidden "$1"
    fi
}

################################################################################
# Emacs.
################################################################################

# Create emacs server and connect to server
EMACS_DEFAULT_DAEMON="emacs-server"
EMACS_DEFAULT_SIZE="(set-frame-size (selected-frame) 100 40)"

emacs-start() {
    name=$1
    if [ -z "$name" ]; then
        name="$EMACS_DEFAULT_DAEMON"
    fi
    emacs --daemon=$name
}

emacs-stop() {
    name=$1
    if [ -z "$name" ]; then
        name="$EMACS_DEFAULT_DAEMON"
    fi
    emacsclient -s $name -e "(kill-emacs)"
}

emacs-list-servers() {
    ls -1 /run/user/1000/emacs
}

emacs-connect-new() {
    name=$1
    file=$2
    if [ -z "$2" ]; then
        name="$EMACS_DEFAULT_DAEMON"
        file="$1"
    fi
    if [ -z "$file" ]; then
        nohup emacsclient -c -s "$name" -e "$EMACS_DEFAULT_SIZE" > /dev/null 2>&1 & disown
    else
        nohup emacsclient -c -s "$name" -e "$EMACS_DEFAULT_SIZE" "(find-file \"$file\")" > /dev/null 2>&1 & disown
    fi
}

emacs-connect() {
    name=$1
    file=$2

    if [ -z "$2" ]; then
        name="$EMACS_DEFAULT_DAEMON"
        file="$1"
    fi

    window_count=$( emacsclient -s $name -e "(length (frame-list))" )

    if [ -z "$file" ]; then
        if [ "$window_count" = "1" ]; then
            nohup emacsclient -c -s "$name" -e "$EMACS_DEFAULT_SIZE" > /dev/null 2>&1 & disown
        else
            nohup emacsclient -s "$name" > /dev/null 2>&1 & disown
        fi
    else
        if [ "$window_count" = "1" ]; then
            nohup emacsclient -c -s "$name" -e "$EMACS_DEFAULT_SIZE" "(find-file \"$file\")" > /dev/null 2>&1 & disown
        else
            nohup emacsclient -s "$name" -e "(find-file \"$file\")" > /dev/null 2>&1 & disown
        fi
    fi
}
