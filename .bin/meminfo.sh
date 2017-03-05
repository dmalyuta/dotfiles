#!/bin/sh
RAM=`cat /proc/meminfo | grep "MemFree" | awk -F" " '{print $2}'`
SWAP=`cat /proc/meminfo | grep "SwapFree" | awk -F" " '{print $2}'`
echo -n "${RAM}kb/ram ${SWAP}kb/swap"
