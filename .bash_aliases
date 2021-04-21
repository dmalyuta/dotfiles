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

# Display hardware list
alias abouthw='sudo lshw -short'

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

# Git GUI viewer
gitview() {
    nohup gitg $@ &>/dev/null & disown
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

# Get line of output
getline() {
    sed -n "$@"p
}

# Show line numbers in standard output
# E.g. $ findfile 'foobar' | showlinenumbers
alias showlinenumbers='cat --number'

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

# Shortcut Ctrl+Alt+F for Firefox
python ~/.bin/set_customshortcut.py 'launch-brave' 'brave-browser' '<Ctrl><Alt>B'

# Shortcut Ctrl+Alt+E for Emacs
python ~/.bin/set_customshortcut.py 'launch-emacs' 'emacs' '<Ctrl><Alt>M'

# Shortcut Shift+Ctrl+Alt+N for screen color inverse
python ~/.bin/set_customshortcut.py 'invert-color' 'xcalib -invert -alter' '<Shift><Ctrl><Alt>N'

# Rofi desktop searching
python ~/.bin/set_customshortcut.py 'launch-rofi' \
       "rofi -modi drun,window,find:~/.bin/finder.sh -show drun" '<Ctrl><Alt>R'
