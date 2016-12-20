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

make_symlink()
{
    local name="$1"
    local origin="$2"
    local destination="$3"
    local do_symlink=$4 # whether to copy or create symlink
    
    origin_version="${origin}/$name"
    destination_version="${destination}/$name"
    if [ ! -e "$origin_version" ]; then
	echoerr "couldn't find $origin_version"
	builtin echo
	echo_warnings_errors
	builtin echo
	exit 1
    fi
    destination_version_parent_dir="$(dirname "$destination_version")"
    makefolder "$destination_version_parent_dir"
    if $do_symlink; then
	# create a symlink in destination, instead of copying
	runcmd "eval /bin/cp -asrf --remove-destination \"$origin_version\" \"${destination_version_parent_dir}\""
    else
	# copy to destination
	runcmd "eval /bin/cp -arf --remove-destination \"$origin_version\" \"${destination_version_parent_dir}\""
    fi
}

copy_foo()
{
    local foo="$1" # file or folder name
    local origin="$2" # origin directory
    local destination="$3" # destination directory
    local do_symlink=$4 # whether to copy or create symlink

    local origin_foo="${origin}/$foo"
    local destination_foo="${destination}/$foo"
    
    if [ -e "$origin_foo" ]; then
	# back up existing (destination) foo
	if [ -e "$destination_foo" ]; then
	    # backup $destination_foo
	    runcmd "cp -aLr $destination_foo $backup_folder"
	    runcmd "rm -rf $destination_foo"
	fi
	# copy or symlink the git file to the place in $HOME
	make_symlink "$foo" "$origin" "$destination" $do_symlink
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

    copy_foo "$foo" "$source" "$destination" false
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
    runcmd "tar -zxvf ${home}/Downloads/${name}.tar.gz -C ${home}/Downloads"
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
