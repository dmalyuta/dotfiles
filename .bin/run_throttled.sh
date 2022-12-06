#!/bin/bash
#
# Runs a process with limited resources. For example:
#    run_throttled 3000M 100% /usr/bin/chrome

RAM_LIMIT="$1"
CPU_LIMIT="$2"
BINARY="$3"

systemd-run --scope -p MemoryLimit="${RAM_LIMIT}" -p CPUQuota="${CPU_LIMIT}" "${BINARY}" \
    > /dev/null 2>&1 & disown
