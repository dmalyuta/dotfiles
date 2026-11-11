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
alias nap='systemctl suspend'
alias dotfiles='cd ~/Projects/dotfiles && ll'
alias rgb_on='openrgb --profile blue'
alias rgb_off='openrgb --profile off'
alias fix_audio='systemctl --user restart pipewire pipewire-pulse wireplumber'
alias pdfx_wayland='run-detached env -u DISPLAY WINEPREFIX=~/.wine wine "$HOME/.wine/drive_c/Program Files/PDF-XChange/PDF Editor/PXCEditor.exe"'

################################################################################
# Process management.
################################################################################

killgrep() {
  ps -ef | grep -E "$*" | grep -v grep | awk '{print $2}' | xargs -r kill
}

psgrep() {
  ps aux | grep -E "$*"
}

# Run a command detached: no terminal blocking, survives terminal close.
# Default is literal (spaces in arguments are safe):
#   run-detached xdg-open 'My File.pdf'
# Use -c for shell syntax like pipes and redirects (quote the whole command):
#   run-detached -c 'foo | bar > out.log'
run-detached() {
  if [[ $1 == -c ]]; then
    shift
    setsid nohup bash -c "$*" >/dev/null 2>&1 &
  else
    setsid nohup "$@" >/dev/null 2>&1 &
  fi
  disown
}

################################################################################
# Navigation.
################################################################################

cs() {
  cd "$1" && ll
}

################################################################################
# Search.
################################################################################

# Each takes a name pattern, then extra `find` options, and searches below the current folder.
find_file() {
  local name=$1
  shift
  find ~+ "$@" -type f -name "$name" 2>&1 | grep -v 'Permission denied'
}

find_symlink() {
  local name=$1
  shift
  find ~+ "$@" -type l -name "$name"
}

find_file_or_symlink() {
  echo "=== Files ==="
  find_file "$@"
  echo ""
  echo "=== Symlinks ==="
  find_symlink "$@"
}

find_folder() {
  local name=$1
  shift
  find ~+ "$@" -type d -name "$name"
}

# find_string PATTERN, or find_string FILE_REGEX PATTERN to search only files whose name matches.
find_string() {
  if [ $# -eq 2 ]; then
    ag --follow --hidden -G "$1" "$2"
  else
    ag --follow --hidden "$1"
  fi
}
