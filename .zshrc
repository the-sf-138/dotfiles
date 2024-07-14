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

source /home/the_sf/src/dotfiles/colemak-mode.zsh
autoload -z edit-command-line
zle -N edit-command-line
bindkey -M vicmd "v" edit-command-line

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
export PATH="$PATH:/home/the_sf/.config/emacs/bin"
#setxkbmap us -variant colemak

export EDITOR=emacsclient

export PATH=$PATH:/home/the_sf/bin/


fpath=(~/.zsh/completions $fpath)


_codeforces_completion() {
    _arguments '--contest-number[ file]:xxx:->argument1' '--problem[problem letter]:xxx:->argument2'
}

function codeforces() {
    /home/the_sf/src/infinite-th-cf-parser/main.py $@
}

compdef _codeforces_completion codeforces

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/usr/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/usr/etc/profile.d/conda.sh" ]; then
        . "/usr/etc/profile.d/conda.sh"
    else
        export PATH="/usr/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

my_cd_up() {
  cd ..
}

zle -N my_cd_up
bindkey '^T' my_cd_up


alias open="xdg-open"

bindkey '^r' history-incremental-search-backward
