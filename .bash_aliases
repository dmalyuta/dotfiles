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

# Join PDF files into one
# Call: $ pdfjoin <in1.pdf> [in2.pdf ...] <out.pdf>
pdfjoin() {
    local length=$(($#-1))
    local input_pdf=("${@: 1:$length}")
    local output_pdf=("${@: -1}")
    pdftk "${input_pdf[@]}" cat output "${output_pdf[@]}"
}

# Kill processes matching grep
alias killgrep='~/.bin/killgrep.sh'

# Open file or URL in default application
alias easyopen='xdg-open'

# Restart networking
alias restart-network='sudo systemctl restart NetworkManager.service'

# Directory making
now() { date "+%d%m%YT%H%M%S"; } # shortcut for timestamps

# Gedit open file without blocking terminal
gedit() {
    nohup gedit $@ &>/dev/null & disown
}

# Emacs open file without blocking terminal
semacs() {
    nohup emacs $@ &>/dev/null & disown
}
semacsd() {
    semacs
    exit
}

# LibreOffice open file without blocking terminal
sdoc() {
    nohup libreoffice6.4 $@ &>/dev/null & disown
}

# Evince open file without blocking terminal
sevince() {
    nohup evince $@ &>/dev/null & disown
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

# Git push
gitpush() {
    pswd github.com
    git push
}
gitpushoverleaf() {
    pswd overleaf.com
    git push
}

# Shortcut Ctrl+Alt+F for Firefox
python ~/.bin/set_customshortcut.py 'launch-firefox' 'firefox' '<Ctrl><Alt>F'

# Shortcut Ctrl+Alt+E for Emacs
python ~/.bin/set_customshortcut.py 'launch-emacs' 'emacs' '<Ctrl><Alt>E'
# Show kernel log
# See for more info: https://www.digitalocean.com/community/tutorials/how-to-use-journalctl-to-view-and-manipulate-systemd-logs
alias viewkernel='journalctl -k --no-pager'

# Show sleep mode
# For most battery savings during sleep, the output should be (deep sleep):
# $ s2idle [deep]
alias sleep_mode='cat /sys/power/mem_sleep'

# Invert screen colors
alias neg='xcalib -invert -alter'

# Shortcut for screen color inverse
python ~/.bin/set_customshortcut.py 'invert-color' 'xcalib -invert -alter' '<Shift><Ctrl><Alt>N'

# Start Emacs server for Git
emacsgit() {
    if ! (ps aux | grep -E "[e]macs.*git.*--daemon" > /dev/null 2>&1); then
	~/.bin/emacs_git.sh
    fi
}

# Find files and folders
alias findfile='find . -type f -name'
alias finddir='find . -type d -name'

# Get line of output
getline() {
    sed -n "$@"p
}

# ls with directories first when `ls -1`
alias ls='ls --color -h --group-directories-first'
