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


