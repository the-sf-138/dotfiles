export ZSH=/home/the_sf/.oh-my-zsh
ZSH_THEME="lambda"
plugins=(git)

source $ZSH/oh-my-zsh.sh
setxkbmap -option ctrl:nocaps
set -o vi
alias clip="xclip -selection c"
alias src="cd ~/src"
alias pub_ip="curl -s icanhazip.com/s"
alias pd="pushd"

bindkey -a h vi-backward-char
bindkey -a n vi-down-line-or-history
bindkey -a e vi-up-line-or-history
bindkey -a i vi-forward-char
bindkey -a / history-incremental-search-backward


# Stuff for vterm
vterm_printf(){
    if [ -n "$TMUX" ]; then
        # Tell tmux to pass the escape sequences through
        # (Source: http://permalink.gmane.org/gmane.comp.terminal-emulators.tmux.user/1324)
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}

if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
    alias clear='vterm_printf "51;Evterm-clear-scrollback";tput clear'
fi 

vterm_prompt_end() {
    vterm_printf "51;A$(whoami)@$(hostname):$(pwd)";
}
setopt PROMPT_SUBST
PROMPT=$PROMPT'%{$(vterm_prompt_end)%}'

export PATH="$PATH:/home/the_sf/.local/bin"
export PATH="$PATH:/home/the_sf/.local/bin/scripts"
#setxkbmap us -variant colemak

export EDITOR=emacsclient
