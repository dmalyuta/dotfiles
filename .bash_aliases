# ~/.bash_alises: my aliases for bash

# Enable using another alias after sudo
alias sudo='sudo '

# make sure the rm command, whatever it is, asks whether you really
# want to delete. Can save from accidentally deleting important
# files/folders.
alias rm='rm -i'

# Clipboard
alias copy='xargs echo -n | xclip -selection clipboard'

# MATLAB terminal
matlab() {
	if [ -f /usr/local/MATLAB/R2017a/bin/matlab ]; then
		/usr/local/MATLAB/R2017a/bin/matlab & 2&>/dev/null && disown # -softwareopengl
	else
		echo "Executable not found :("
	fi
}
matlabterminal() {
	if [ -f /usr/local/MATLAB/R2017a/bin/matlab ]; then
		/usr/local/MATLAB/R2017a/bin/matlab -nodesktop -nosplash
	else
		echo "Executable not found :("
	fi
}

# Directory making
now() { date "+%d%m%YT%H%M%S"; } # shortcut for timestamps

# Grep contents of files in <dir> for <pattern>
alias greptext='grep -rnw'

# Screen
killscreens () {
    screen -ls | grep Detached | cut -d. -f1 | awk '{print $1}' | xargs kill
}

# Jupyter
alias venv_jupnb=". ~/.python_venv/jupnb/bin/activate && echo Use \'$ deactivate\' to quit the Jupyter notebook virtualenv"

# JetBrains products
clion() {
    # Launch Clion (C/C++)
    if [ -f ~/.jetbrains/clion/bin/clion.sh ]; then
		~/.jetbrains/clion/bin/clion.sh &>/dev/null & disown
	else
		echo "Executable not found :("
	fi
}
pycharm() {
    # Launch PyCharm (Python)
    if [ -f ~/.jetbrains/pycharm/bin/pycharm.sh ]; then
		~/.jetbrains/pycharm/bin/pycharm.sh &>/dev/null & disown
	else
		echo "Executable not found :("
	fi
}

# Shortcut video editor
shotcut () {
	if [ -f ~/.shotcut/Shotcut.app/shotcut ]; then
		~/.shotcut/Shotcut.app/shotcut &>/dev/null & disown
	else
		echo "Executable not found :("
	fi
}

# Gedit open file without blocking terminal
gedit() {
    nohup gedit $@ &>/dev/null & disown
}

# Eclipse editors
eclipse_common() {
    # Launch Eclipse CDT (C/C++, Java, XML, etc.)
    if [ -f ~/.eclipse/eclipse_common/eclipse ]; then
		UBUNTU_MENUPROXY=0 ~/.eclipse/eclipse_common/eclipse &>/dev/null & disown
	else
		echo "Executable not found :("
	fi
}
eclipse_mbse() {
    # Launch Eclipse (model-based systems engineering with UML/SysML)
    if [ -f ~/.eclipse/eclipse_mbse/eclipse ]; then
		UBUNTU_MENUPROXY=0 ~/.eclipse/eclipse_mbse/eclipse &>/dev/null & disown
	else
		echo "Executable not found :("
	fi
}
eclipse_latex() {
    # Launch Eclipse (LaTeX)
    if [ -f ~/.eclipse/eclipse_latex/eclipse ]; then
		UBUNTU_MENUPROXY=0 ~/.eclipse/eclipse_latex/eclipse &>/dev/null & disown
	else
		echo "Executable not found :("
	fi
}
lpcxpresso() {
    # Launch LPCXpresso (IDE from NXP for LPC microcontrollers)
    if [ -f /usr/local/lpcxpresso_8.2.2_650/lpcxpresso/lpcxpresso ]; then
		UBUNTU_MENUPROXY=0 /usr/local/lpcxpresso_8.2.2_650/lpcxpresso/lpcxpresso &>/dev/null & disown
	else
		echo "Executable not found :("
	fi
}

# Jupyter
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

# yEd
yed() {
    java -jar ~/.yed/yed.jar &>/dev/null & disown
}
