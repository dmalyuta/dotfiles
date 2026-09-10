#!/bin/bash
#
# Functions for working with and opening tmux.

open_tmux() {
    # Opens tmux to a currently unused window, or a new one.
    if command -v tmux &> /dev/null && \
            [[ -n "$PS1" && \
                ! "$TERM" =~ screen && \
                ! "$TERM" =~ tmux && \
                (-z "$TMUX" || -n "$INSIDE_VSCODE") && \
                # Solution for "Unable to resolve your shell environtment"
                # https://github.com/microsoft/vscode/issues/135166#issuecomment-947185908
                # But the INVOCATION_PID is set in a regular shell as well. Better to use VSCODE_PID as
                # suggested from the list of environment variables set by VS Code:
                # https://github.com/microsoft/vscode/issues/134864#issuecomment-945037189
                ( (-z "$VSCODE_PID" && -z "$INSIDE_VSCODE") || \
                    (-n "$INSIDE_VSCODE" && \
                        (-z "$SSH_CLIENT" && -z "$SSH_CONNECTION" && -z "$SSH_wTTY") ) ) ]]; then
        if tmux ls 2> /dev/null | grep -q -v attached; then
            # Reclaim an existing, detached session
            exec tmux attach -t $(tmux ls 2> /dev/null | grep -v attached | head -1 | cut -d : -f 1)
        else
            exec tmux
        fi
    fi
}
