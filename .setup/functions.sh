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
    local cmd="$1"
    echo "$cmd"
    if [ $dryrun = false ]; then
	if [ "$2" ]; then
	    # disable 1>/dev/null which prevents from writing to file
	    $cmd
	else
	    $cmd 1>/dev/null
	fi
	local exit_status=$?
	if [ $exit_status -ne 0 ]; then
	    echoerr "the last command failed (exit status ${exit_status}), please see why and rerun the script"
	    echo_warnings_errors
	    exit 1
	fi
    fi
}

subshell_check()
{ # exit script if subshell failed
    local subshell_exit_status="$1"
    if [[ "$subshell_exit_status" -ne 0 ]]; then
	exit $subshell_exit_status
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
    local foo="$1" # file or folder name
    local dir="$2" # root directory of dotfiles folder
    local home="$3" # home directory

    local git_foo="${dir}/$foo" # new folder from repository
    local home_foo="${home}/$foo" # existing folder in ${HOME}
    
    if [ -e "$git_foo" ]; then
	# move existing foo to backup folder
	if [ -e "$home_foo" ]; then
	    # backup $home_foo
	    runcmd "mv $home_foo $backup_folder"
	fi

	# make the full path of files to be copied
	home_foo_parent_dir="$(dirname "$home_foo")"
	if [ -d "$git_foo" ]; then
	    # if foo is a folder, prepare the folder
	    makefolder "$home_foo"
	else
	    # if foo is a file, prepare its parent folder
	    makefolder "$home_foo_parent_dir"
	fi
	
	# hard link git repo folder to home (preserves permissions,
	# makes changes synchronized between original and copied file)
	runcmd "cp -al $git_foo $home_foo_parent_dir"
    else
	echoerr "couldn't find $git_foo"
	exit 1
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

    # download and install
    runcmd "wget $url -P ${home}/Downloads/"
    runcmd "tar -zxvf ${home}/Downloads/${name}.tar.gz"
    (runcmd "cd ${home}/Downloads/${name}/" && configure_make_install)
    subshell_check $?

    # remove downloaded files
    runcmd "rm -rf ${home}/Downloads/${name}.tar.gz"
    runcmd "rm -rf ${home}/Downloads/${name}/"
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
