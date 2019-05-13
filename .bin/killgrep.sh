#!/bin/bash
#
# Kills processes matching grep
#
# See https://xmodulo.com/how-to-kill-multiple-processes-at-once-with-grep.html

ARGS=$*

kill -9 `ps aux | grep $ARGS | grep -v grep | awk '{print $2}'`
