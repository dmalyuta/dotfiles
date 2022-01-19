# ~/.bash_alises: my aliases for bash

# Enable using another alias after sudo
alias sudo='sudo '

# make sure the rm command, whatever it is, asks whether you really
# want to delete. Can save from accidentally deleting important
# files/folders.
alias rm='rm -i'

# Clipboard
alias copy='xargs echo -n | xclip -selection clipboard'

# Display processes matching grep
alias listproc='~/.bin/listproc.sh'

# Number of running processes
alias numtasks='ps -e | wc -l'

# Display hardware list
alias abouthw='sudo lshw -short'

# Kill processes matching grep
alias killgrep='~/.bin/killgrep.sh'

# Find files
alias fd='fdfind'

# Open file or URL in default application
alias easyopen='xdg-open'

# Check OOM'ed processes
alias oom-ls='sudo egrep -i -r ''killed process'' /var/log/'

# Open browser
if [ ! -f ~/.local/bin/browser ]; then
    ln -sf /usr/bin/brave-browser ~/.local/bin/browser
fi

# Open logseq easily
alias logseq="logseq.AppImage"
notebook() {
    # Open logseq without blocking the terminal
    logseq > /dev/null 2>&1 &
}
notebookd() {
    # Open logseq and close the terminal
    notebook
    exit
}

# Open a terminal easily
term() {
    # Open terminator without blocking the terminal
    terminator > /dev/null 2>&1 &
}
termd() {
    # Open terminator and close the terminal
    term
    exit
}

# Restart networking
alias restart-network='sudo systemctl restart NetworkManager.service'

# Directory making
now() { date "+%d%m%YT%H%M%S"; } # shortcut for timestamps

# Gedit open file without blocking terminal
gedit() {
    nohup gedit $@ > /dev/null 2>&1 & disown
}

# Emacs open file without blocking terminal
semacs() {
    nohup emacs $@ > /dev/null 2>&1 & disown
}

semacsd() {
    semacs
    exit
}

# Create emacs server and connect to server
EMACS_DEFAULT_DAEMON="emacs-server"

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
        nohup emacsclient -c -s "$name" -e "(raise-frame)" > /dev/null 2>&1 & disown
    else
        nohup emacsclient -c -s "$name" "$file" > /dev/null 2>&1 & disown
    fi
}

emacs-connect() {
    name=$1
    file=$2

    if [ -z "$2" ]; then
        name="$EMACS_DEFAULT_DAEMON"
        file="$1"
    fi

    window_count=$( emacsclient -s emacs-server -e "(length (frame-list))" )

    if [ -z "$file" ]; then
        if [ "$window_count" = "1" ]; then
            nohup emacsclient -c -s "$name" -e "(raise-frame)" > /dev/null 2>&1 & disown
        else
            nohup emacsclient -s "$name" -e "(raise-frame)" > /dev/null 2>&1 & disown
        fi
    else
        if [ "$window_count" = "1" ]; then
            nohup emacsclient -c -s "$name" "$file" > /dev/null 2>&1 & disown
        else
            nohup emacsclient -s "$name" "$file" > /dev/null 2>&1 & disown
        fi
    fi
}

# LibreOffice open file without blocking terminal
sdoc() {
    nohup libreoffice6.4 $@ > /dev/null 2>&1 & disown
}

# Evince open file without blocking terminal
sevince() {
    nohup evince $@ > /dev/null 2>&1 & disown
}

# Emacs byte-compile
emacs-byte-compile() {
    emacs -batch -f batch-byte-compile $@
}

# Jupyter
alias venv_jupnb=". ~/.python_venv/jupnb/bin/activate && echo Use \'$ deactivate\' to quit the Jupyter notebook virtualenv"
alias jn='jupyter-notebook '
alias jl='jupyter lab '
alias jc='jupyter nbconvert '
jupyter2pdf() {
    # Convert .ipynb to .pdf
    if [ "$#" -ne 1 ]; then
        echo "Usage: jupyter2pdf <JUPYTER_NOTEBOOK_NAME>.ipynb"
	    return 1
    fi
    local notebook_name_original="$1"
    notebook_name=${notebook_name_original%".ipynb"}
    if [ "$notebook_name_original" == "$notebook_name" ]; then
        echo "Usage: jupyter2pdf <JUPYTER_NOTEBOOK_NAME>.ipynb"
        return 1
    fi
    jupyter nbconvert --to html "${notebook_name}.ipynb" --output=tmpfile123
    wkhtmltopdf --enable-internal-links tmpfile123.html "${notebook_name}.pdf"
    rm -f tmpfile123.html
}

# Laspass
passwordcopy() {
    lpass show "$1" --password --clip
}
alias pswd="passwordcopy"

# Git commands
alias git-print-branch="git rev-parse --abbrev-ref HEAD"
alias git-cd-root="cd \$(git root)"
git-log () {
    git log --oneline --abbrev-commit --graph --pretty=format:"%C(yellow)%h %Cred%>(12)%ar %Cgreen%<(7)%aN%Cred%d %Creset%s"
}

# Git GUI viewer
gitview() {
    nohup gitg $@ > /dev/null 2>&1 & disown
}

# Show kernel log
# See for more info: https://www.digitalocean.com/community/tutorials/how-to-use-journalctl-to-view-and-manipulate-systemd-logs
alias viewkernel='journalctl -k --no-pager'

# Show sleep mode
# For most battery savings during sleep, the output should be (deep sleep):
# $ s2idle [deep]
alias sleep_mode='cat /sys/power/mem_sleep'

# Invert screen colors
alias neg='xcalib -invert -alter'

# Start Emacs server for Git
emacsgit() {
    if ! (ps aux | grep -E "[e]macs.*git.*--daemon" > /dev/null 2>&1); then
	~/.bin/emacs_git.sh
    fi
}

# Connect to the Emacs server for Git (opens a GUI frame)
alias emacsgitconnect='emacsclient --create-frame --socket-name=git'

# Find files and folders
alias findfile='find . -type f -name'
alias finddir='find . -type d -name'

# Show lines or get line of output
# Call:
#   $ <command> | lines
#   $ <command> | lines <number>
lines() {
    local length=$(($#))
    if [ $length -eq 0 ]; then
        cat --number
    elif [ $length -eq 1 ]; then
        sed -n "$1"p | sed 's/^[ \t]\+[0-9]\+[ \t]\+//' | sed -e 's/^[[:space:]]*//'
    else
        echo "Incorrect usage of lines commands. Possible usage:"
        echo "  lines"
        echo "  lines <number>"
    fi
}

# ls with directories first when `ls -1`
alias ls='ls --color -h --group-directories-first'

# ..:: Working with PDF ::..

# Join PDF files into one
# Call: $ pdfjoin <in1.pdf> [in2.pdf ...] <out.pdf>
pdfjoin() {
    local length=$(($#-1))
    local input_pdf=("${@: 1:$length}")
    local output_pdf=("${@: -1}")
    pdftk "${input_pdf[@]}" cat output "${output_pdf[@]}"
}

# Extract PDF pages
# Call: $ pdfgetpages <in.pdf> <page_start>[-page_end] <out.pdf>
pdfgetpages() {
    local length=$(($#-2))
    local input_pdf=("$1")
    local pages=("${@: 2:$length}")
    local output_pdf=("${@: -1}")
    if [ "$input_pdf" = "$output_pdf" ]; then
	local random_string=$(tr -dc A-Za-z0-9 </dev/urandom | \
				  head -c 13 ; echo '')
	local output_pdf_tmp="${random_string}${output_pdf}"
	pdftk "$input_pdf" cat "${pages[@]}" output "$output_pdf_tmp"
	mv "$output_pdf_tmp" "$input_pdf"
	rm -rf "$output_pdf_tmp"
    else
	pdftk "$input_pdf" cat "${pages[@]}" output "$output_pdf"
    fi
}

# Rotate all pages +90 degrees
# Call: $ pdfrotate <in.pdf>
pdfrotate() {
    local input_pdf="$1"
    pdftk "$input_pdf" cat 1-endright output /tmp/"$input_pdf".pdf
    mv /tmp/"$input_pdf".pdf "$input_pdf"
}

# Reverse page order
# Call: $ pdfreverse <in.pdf>
pdfreverse() {
    local input_pdf="$1"
    pdftk "$input_pdf" cat end-1 output /tmp/"$input_pdf".pdf
    mv /tmp/"$input_pdf".pdf "$input_pdf"
}

# Interleave even and odd pages
# Call: $ pdfinterleave <odd.pdf> <even.pdf> <out.pdf>
pdfinterleave() {
    local odd_pdf="$1"
    local even_pdf="$2"
    local output_pdf="$3"
    pdftk A="$odd_pdf" B="$even_pdf" shuffle A B output "$output_pdf"
}

# ..:: Shortcuts ::..
# You can clear these by doing:
# $ gsettings set org.gnome.settings-daemon.plugins.media-keys custom-keybindings []

# Shortcut Ctrl+Alt+B for web browser
python ~/.bin/set_customshortcut.py 'launch-browser' 'browser' '<Ctrl><Alt>B'

# Shortcut Shift+Ctrl+Alt+N for screen color inverse
python ~/.bin/set_customshortcut.py 'invert-color' 'xcalib -invert -alter' '<Shift><Ctrl><Alt>N'

# Rofi desktop searching
python ~/.bin/set_customshortcut.py 'launch-rofi' \
       "rofi -modi drun,window,find:~/.bin/finder.sh -show drun" '<Ctrl><Alt>R'

# Flameshot screenshot
python ~/.bin/set_customshortcut.py 'take-screenshot' \
       "/usr/bin/flameshot gui" '<Shift><Ctrl><Alt>P'
