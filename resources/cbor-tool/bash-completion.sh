#!/bin/sh
_cbor_tool()
{
    local cmdline
    local IFS=$'
'
    CMDLINE=(--bash-completion-index $COMP_CWORD)

    for arg in ${COMP_WORDS[@]}; do
        CMDLINE=(${CMDLINE[@]} --bash-completion-word $arg)
    done

    COMPREPLY=( $(cbor-tool "${CMDLINE[@]}") )
}

complete -o filenames -o noquote -F _cbor_tool cbor-tool
