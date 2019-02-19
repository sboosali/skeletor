_skeletor-haskell()
{
    local CMDLINE
    local IFS=$'\n'
    CMDLINE=(--bash-completion-index $COMP_CWORD)

    for arg in ${COMP_WORDS[@]}; do
        CMDLINE=(${CMDLINE[@]} --bash-completion-word $arg)
    done

    COMPREPLY=( $(skeletor-haskell "${CMDLINE[@]}") )
}

complete -o filenames -F _skeletor-haskell skeletor-haskell
