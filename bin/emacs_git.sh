#!/bin/bash
#
# Run Emacs server for Git

check_emacs_server_is_running() {
    local serverdir="${TMPDIR:-/tmp}/emacs${UID}"
    local -a servers
    for file in ${serverdir}/*; do
	if [[ -S ${file} ]]; then
	    server_name="${file##*/}"
	    if [[ $server_name == "git" ]]; then
		echo 0
		return
	    fi
	fi
    done
    echo 1
}

if [[ $(check_emacs_server_is_running) == 1 ]]; then
    emacs --eval "(setq server-name \"git\")" --daemon
fi
