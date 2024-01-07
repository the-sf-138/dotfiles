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
export PATH="$PATH:/home/the_sf/.config/emacs/bin"
#setxkbmap us -variant colemak

export EDITOR=emacsclient

export PATH=$PATH:/home/the_sf/bin/


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


fpath=(~/.zsh/completions $fpath)


function test_tim() {
    echo "Selected $1"
}


_test_tim_files_completion() {
    _arguments '-m[music file]:filename:->files' '-f[flags]:flag:->flags'
    case "$state" in
        files)
            local -a music_files
            music_files=( $(ls ~/test-files/) )
            _values 'music files' $music_files
            ;;
        flags)
            _values -s , 'flags' a b c d e
            ;;
    esac
}

compdef _test_tim_files_completion test_tim
