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
--preview-window=bottom,5,wrap"

################################################################################
# Flyline.
################################################################################

# Enables the flyline builtin and configures it. Kept in its own file because
# flyline has no config file of its own -- every setting is a command that has
# to re-run in each shell.
if [ -f ~/.flyline.conf ]; then
    . ~/.flyline.conf
fi
