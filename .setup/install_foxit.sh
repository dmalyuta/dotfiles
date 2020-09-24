#!/bin/bash
# ----------------------------------------------------------------------
#
# Foxit PDF Reader installation.
#
#
# View, create, convert, annotate and print. Collaborate and
# share. Fill Forms. Sign. Probably the best standardized PDF
# capability for Linux!
#
# To learn more about Foxit PDF Reader, visit:
# https://www.foxitsoftware.com/products/pdf-reader/
#
# ----------------------------------------------------------------------

echo_prefix_temp="$echo_prefix"
echo_prefix="[foxit setup] "

if program_not_installed "FoxitReader"; then    
    # download Foxit Reader
    bit_number="$(uname -m)"
    if [[ "$bit_number" == "x86_64" ]]; then
		# 64-bit
		echo "Detected that you're running a 64-bit OS, will install 64-bit version of Foxit PDF Reader"
		runcmd "wget -4 https://www.foxitsoftware.com/downloads/latest.php?product=Foxit-Reader&platform=Linux-64-bit&version=2.4.0.14978&package_type=run&language=English -O /tmp/foxit.tar.gz"
    else
		# 32-bit
		echo "Detected that you're running a 32-bit OS, will install 32-bit version of Foxit PDF Reader"
		runcmd "wget -4 https://www.foxitsoftware.com/downloads/latest.php?product=Foxit-Reader&platform=Linux-32-bit&version=2.4.0.14978&package_type=run&language=English -O /tmp/foxit.tar.gz"
    fi

    # unpack the installer
    runcmd "tar -zxvf /tmp/foxit.tar.gz -C /tmp"
    
    foxit_installer_name=$(ls /tmp/FoxitReader*.run)
	
    # install Foxit PDF Reader
    echowarn "Please follow the GUI's instructions and install Foxit PDF Reader into /opt/foxitsoftware/foxitreader"
    runcmd "eval '${foxit_installer_name}'"
    installer_exit_status=$?

    # delete the installer
    runcmd "rm -f $foxit_installer_name"

    if [[ $installer_exit_status -eq 0 ]]; then
	# installation finished successfully (i.e. user did not quit it)
	
	# disable the Foxit PDF Reader's cloud tools, which use lots of CPU, by moving to the fxpluging_old directory
	echo "Will now disable Foxit PDF Reader's cloud tools, which can needlessly consume a lot of CPU."    
	fxplugins_dir="/opt/foxitsoftware/foxitreader/fxplugins"
	if [ ! -d "${fxplugins_dir}" ]; then
	    echowarn "Looks like you did not install Foxit PDF Reader in /opt/foxitsoftware/foxitreader/"
	    echo "Searching for fxplugins folder location..."

	    fxplugins_search_results=( $(find / -type d \( -path /root -o -path /mnt -o -path /media \) -prune -o -name '*fxplugins' -print 2>/dev/null) )
	    number_matches=${#fxplugins_search_results[@]}

	    if [[ $number_matches -eq 0 ]]; then
		echowarn "Couldn't find any fxplugins folder, skipping this step"
		skip_fxplugins=true
	    else    
		echo "Found $number_matches matches:"
		builtin echo
		counter=1
		for element in "${fxplugins_search_results[@]}"
		do
		    echo "   ${counter}) $element"
		    ((++counter))
		done
		builtin echo
		flush_stdin
		while true
		do
		    printf_prompt "Enter search result number [1-${number_matches}] of the correct directory for your Foxit PDF Reader installation or type [quit] to skip this step: "
		    read -r -p "" user_response

		    reg_exp='^-?[0-9]+([.][0-9]*)?$'
		    if [[ "$user_response" =~ $reg_exp ]]; then
			# user entered a number
			if [[ "$user_response" -le $number_matches ]] && [[ "$user_response" -ge 1 ]]; then
			    element_choice=$((user_response-1))
			    fxplugins_dir="${fxplugins_search_results[element_choice]}"
			    break
			fi
		    else
			# user entered a string
			if [[ "$user_response" =~ ^[qQ][uU][iI][tT]$ ]]; then
			    skip_fxplugins=true
			    break
			fi
		    fi
		done
	    fi
	fi

	if [[ -z $skip_fxplugins ]]; then
	    # move Foxit PDF Reader cloud tools to fxplugins_old directory, which effectively disables them without deleting them
	    runcmd "mv ${fxplugins_dir} ${fxplugins_dir}_old"
	    runcmd "mkdir -p ${fxplugins_dir}"

	    # give ownership of Foxit PDF Reader to the normal user
	    foxit_base_install_dir="$(dirname "$fxplugins_dir")"
	    runcmd "chown -R ${SUDO_USER}:${SUDO_USER} $foxit_base_install_dir"
	else
	    echoerr "Failed to disable Foxit PDF Reader's cloud tools. Beware: these may consume a lot of CPU! You are advised to remove them."
	    echoerr "Failed to give ownership of Foxit PDF Reader to the normal user. You have to manually run \`sudo chown -R ${SUDO_USER}:${SUDO_USER} <foxit reader install directory>\` to make the program work normally."
	fi
    else
	echoerr "You quit the Foxit PDF Reader installer... won't install it."
    fi
fi

echo_prefix="$echo_prefix_temp"
