# ~/.top.bashrc: sourced at the top of .bashrc for early actions. Put the
# following at the top of your ~/.bashrc:
#
# [ -f ~/.top.bashrc ] && source ~/.top.bashrc
# if [ $EARLY_OUT -eq 1 ]; then
#     return
# fi
# unset EARLY_OUT

EARLY_OUT=1

# If not running interactively, don't to anything.
case $- in
*i*) ;;
*) return ;;
esac

# Early-out for TRAMP shells in Emacs.
IS_TRAMP=0
if [[ "$TERM" = "dumb" ]]; then
    PS1="$ "
    IS_TRAMP=1
    return
fi

EARLY_OUT=0
