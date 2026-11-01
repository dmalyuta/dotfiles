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
alias rgb_on='~/AppImages/openrgb.appimage --profile ~/.config/OpenRGB/danymal-blue.orp'
alias rgb_off='~/AppImages/openrgb.appimage --profile ~/.config/OpenRGB/danymal-off.orp'
alias fix_audio='systemctl --user restart pipewire pipewire-pulse wireplumber'

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

# Run a command detached: no terminal blocking, survives terminal close.
# Default is literal (spaces in arguments are safe):
#   run_detached xdg-open 'My File.pdf'
# Use -c for shell syntax like pipes and redirects (quote the whole command):
#   run_detached -c 'foo | bar > out.log'
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
