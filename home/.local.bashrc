# ~/.local.bashrc: sourced from .bashrc in order to separate default
# .bashrc file from personal customizations.

################################################################################
# Binaries.
################################################################################

# The dotfiles repo's bin/ (power-info, ...), linked by start_fresh.sh.
export PATH=$PATH:~/.bin

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

################################################################################
# Conda.
################################################################################

# Replaces the block `conda init` writes, which starts Python twice and delays
# every prompt by ~1 s. conda.sh only defines the `conda` shell function. Base's
# binaries go on PATH instead of a full `conda activate base`, so there is no
# "(base)" marker and base's activate.d hooks don't run.
if [ -f ~/anaconda3/etc/profile.d/conda.sh ]; then
    . ~/anaconda3/etc/profile.d/conda.sh
    export PATH=~/anaconda3/bin:$PATH
fi
