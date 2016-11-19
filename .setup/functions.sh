#!/bin/bash
# ----------------------------------------------------------------------
#
# Functions to support install scipt.
#
# ----------------------------------------------------------------------

echo()
{ # echo with a custom prefix, if set
    builtin echo -e "${green}${echo_prefix}${nostyle}$1"
}

printf()
{ # printf with a custom prefix, if set
    builtin printf "%b%s%b%s" "${green}" "${echo_prefix}" "${nostyle}" "$1"
}

printf_prompt()
{ # printf with a custom prefix, if set, and user prompt
    builtin printf "%b%s%b%s" "${cyanbold}" "[user input] " "${nostyle}" "$1"
}

echoerr()
{ # output to STDERR (standard error stream)
    builtin echo
    builtin echo -e "${redbold}[error]${nostyle} $1" 1>&2
    builtin echo
    ((++number_errors))
}

echowarn()
{ # output a warning
    builtin echo
    builtin echo -e "${yellowbold}[warning]${nostyle} $1"
    builtin echo
    ((++number_warnings))
}

echo_warnings_errors()
{ # print out how many warnings and errors were encountered
    echo "finished with ${yellowbold}${number_warnings} warnings${nostyle} and ${redbold}${number_errors} errors${nostyle}"
}

runcmd()
{ # run a command in active mode, just print it in dry run mode
    if [ "$2" ]; then
	# disable 1>/dev/null which prevents from writing to file
	local cmd="$1"
    else
	local cmd="$1 1>/dev/null"
    fi
    echo "$1"
    if [ $dryrun = false ]; then
	eval $cmd
	local exit_status=$?
	if [ $exit_status -ne 0 ]; then
	    echoerr "the last command failed (exit status ${exit_status}), please see why and rerun the script"
	    echo_warnings_errors
	    exit 1
	fi
    fi
}

install_program()
{ # install a 
    local program_name="$1"
    local prog_script="$2"

    if [[ ! -z ${install_programs_list["$program_name"]} ]]; then
	source $prog_script
    fi
}

apt_get_install_pkg()
{ # install package with apt-get
    local pkg="$1"
    runcmd "apt-get --assume-yes install $pkg"
}

copy_foo()
{
    local foo="$1"
    local dir="$2"
    local dest="$3"
    
    local git_foo="${dir}/$foo" # new folder from repository
    local home_foo="${dest}/$foo" # existing folder in ${HOME}
    if [ -e "$git_foo" ]; then
	# move existing foo to backup folder
	if [ -e "$home_foo" ]; then
	    runcmd "mv $home_foo $backup_folder"
	else
	    # make the parent directory of the file/folder to be
	    # copied if it does not exist already
	    home_foo_parent_dir="$(dirname "$home_foo")"
	    if [ ! -e "$home_foo_parent_dir" ]; then
		runcmd "mkdir -p $home_foo_parent_dir"
	    fi
	fi
	# copy git repo folder to home, preserving permissions
	makefolder "$dest"
	runcmd "cp -rp $git_foo $home_foo"
    else
	echowarn "couldn't find $git_foo"
    fi
}

makefolder()
{
    local folder="$1"
    if [ ! -d "$folder" ]; then
	runcmd "mkdir -p $folder"
    fi
}

move_foo()
{
    local foo="$1"
    local source="$2"
    local destination="$3"

    copy_foo "$foo" "$source" "$destination"
    runcmd "rm -rf ${source}/${foo}"
}

configure_make_install()
{
    runcmd "./configure"
    runcmd "make"
    runcmd "make install"
}

wget_targz_install()
{
    local name="$1"
    local url="$2"
        
    runcmd "wget $url"
    runcmd "tar -zxvf ${name}.tar.gz"
    runcmd "rm -rf ${name}.tar.gz"
    (runcmd "cd ${name}/" && configure_make_install)
    runcmd "rm -rf ${name}/"
}

program_not_installed()
{ # return true if program is not installed
    local program_name="$1"
    ! type "$program_name" > /dev/null 2>&1
}

flush_stdin()
{ # flush the input buffer
    while read -r -t 0
    do
	read -r
    done
}
