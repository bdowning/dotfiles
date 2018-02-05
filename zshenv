if [[ -z "$BASE_PATH" ]]; then
    export BASE_PATH="$PATH"
    export PATH="$HOME/bin:/usr/local/bin:$PATH:/usr/local/sbin:/usr/sbin:/sbin"
    export EDITOR=vi
    export VISUAL=vi
    export LC_COLLATE=C
    export LESS=FRX
    export QUOTING_STYLE=literal
fi
