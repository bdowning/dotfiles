# zshrc
# bdowning

setopt extended_history
setopt inc_append_history
setopt share_history
setopt hist_expire_dups_first
setopt hist_find_no_dups

SAVEHIST=10000
HISTSIZE=12500
HISTFILE=~/.zsh_history

setopt no_extended_glob
setopt correct_all

# custom bindings
bindkey -e
bindkey '^W' kill-region
setopt no_flow_control

# user functions
#fpath=( $HOME/.zsh/compfuncs $HOME/.zsh/functions $fpath )
#for func in $HOME/.zsh/functions/*; do
#    autoload $(basename $func)
#done
#unset func

# completion
fpath=( $HOME/.zsh/compfuncs $fpath )
setopt no_beep no_auto_menu list_packed
if [[ $USER =~ '^b.*downing$' ]]; then
    # . $HOME/.zcompinstall
    zstyle ':completion:*' verbose yes
    zstyle ':completion:*:descriptions' format '>>>>> %B%d%b <<<<<'
    zstyle ':completion:*:messages' format '%d'
    zstyle ':completion:*:warnings' format 'No matches for: %d'
    zstyle ':completion:*' group-name ''
    zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
    autoload -U compinit
    compinit
fi

# use some bash completion
autoload bashcompinit
bashcompinit
bashcomps=( git )
for comp in $bashcomps; do
    if [ -f /etc/bash_completion.d/$comp ]; then
        source /etc/bash_completion.d/$comp
    fi
done

# directories
setopt cdable_vars
DIRSTACKSIZE=20
setopt auto_pushd pushd_minus pushd_silent pushd_to_home
alias dh='dirs -v'

# job control
setopt no_notify no_hup
alias j='jobs -lp'

# history
#HISTSIZE=5000
alias h='history -Di'
alias h0='h 0'

# other enviornment stuff
alias ls='ls -F'
alias sl=ls

PAGER='less'

if [[ $TERM = dumb ]]; then
    prompt='%m:%~ %!%% '
else
    precmd () { print -rP $'%#%# %B%!%b %B%m%b:%~' }
    case $TERM in
    xterm*|rxvt*|screen)
        precmd () {
            print -Pn "\e]0;%# - %m:%~\a"
            print -P '%#%# %B%!%b %B%m%b:%~'
        }
        preexec () {
            printf "\e]0;%s - %s\a" "$1" "${(%):-'%m:%~'}"
        }
        ;;
    esac
    prompt=':; '
fi

#watch=notme
#WATCHFMT='[%B%T%b] %B%n%b has %a tty %B%l%b%(M: from :)%B%M%b'
#log

#if [ $TERM = "xterm" ]; then
#	stty erase 
#fi

# SBCL and other things need STACK
#ulimit -s 8192

export CVS_RSH=ssh

#function dmalloc { eval `command dmalloc -b $*`; }

alias xo=xdg-open
