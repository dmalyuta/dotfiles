# ~/.local.bashrc: sourced from .bashrc in order to separate default
# .bashrc file from personal customizations.

################################################################################
# Prompt.
################################################################################

if [[ "$TERM" != "dumb" ]]; then
    eval "$(oh-my-posh init bash --config ~/.blue-owl-custom.omp.json)"
fi

##### Fuzzy search.

export FZF_DEFAULT_OPTS="--multi --height=50% --preview 'echo {} |
sed \"s/ @@ /\\n/g\" | bat --color=always --style=numbers -l sh -'
--preview-window=up,5,wrap"

################################################################################
# Emacs.
################################################################################

# Emacs vterm config.
if [[ "$INSIDE_EMACS" = 'vterm' ]] && [[ -f ~/.emacs.d/libvterm/etc/emacs-vterm-bash.sh ]]; then
    # Vterm bash utils.
    source ~/.emacs.d/libvterm/etc/emacs-vterm-bash.sh

    # Directory tracking.
    if [ -n "$SSH_CLIENT" ] || [ -n "$SSH_TTY" ]; then
        vterm_set_directory() {
            vterm_cmd update-pwd "/-:""$USER""@""$HOSTNAME"":""$PWD/"
        }
    else
        vterm_set_directory() {
            vterm_cmd update-pwd "$PWD/"
        }
    fi
    PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND; }vterm_set_directory"

    # Open file in current Emacs session, replacing the vterm window.
    vterm_open_file() {
        vterm_cmd find-file "$(realpath "${@:-.}")"
    }

    # Open file in a window below the vterm window.
    vterm_open_file_below() {
        vterm_cmd find-file-below "$(realpath "${@:-.}")"
    }

    # Print a message.
    say() {
        vterm_cmd message "%s" "$*"
    }
fi
