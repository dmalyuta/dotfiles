#!/bin/bash
# Re-numbers windows so that they all again start from zero and go up in contiguous numbers.
CURRENT_WINDOW=$(tmux display-message -p '#{window_id}')
LAST_WINDOW=-1
tmux list-windows | while read line; do
    WINDOW_NUMBER=$(echo $line | sed 's/^\([0-9]\+\):.*/\1/')
    if (( $WINDOW_NUMBER > $LAST_WINDOW+1 )); then
        tmux move-window -s $WINDOW_NUMBER -t $(( $LAST_WINDOW+1 ))
    fi
    LAST_WINDOW=$(( $LAST_WINDOW+1 ))
done
tmux select-window -t $CURRENT_WINDOW
