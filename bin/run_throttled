#!/bin/bash
#
# Runs a process with limited resources. For example:
#    run_throttled --ram 3000M --cpu 100% /usr/bin/chrome

# Get the binary path.
BINARY="${@: -1}"

# Get the RAM and CPU limits.
RAM_LIMIT="nan"
CPU_LIMIT="nan"
while [ $# -gt 0 ]; do
    case "$1" in
        -c|--cpu) CPU_LIMIT="$2" ; shift 2 ;;
        -r|--ram) RAM_LIMIT="$2" ; shift 2 ;;
        *) shift;
    esac
done

if [ "$RAM_LIMIT" = "nan" ] ||  [ "$CPU_LIMIT" = "nan" ]; then
    cat << EOF
Command:
    run_throttled --cpu <CPU_LIMIT> --ram <RAM_LIMIT> <PATH_TO_BINARY>
EOF
    exit 1
fi

cat << EOF
Running $BINARY with $RAM_LIMIT RAM and $CPU_LIMIT CPU.
EOF

# Pass --user to not ask for a password (https://github.com/systemd/systemd/issues/13340).
systemd-run --user --scope \
    -p MemoryLimit="${RAM_LIMIT}" \
    -p CPUQuota="${CPU_LIMIT}" "${BINARY}" \
    > /dev/null 2>&1 & disown
