export ZSH=/home/timothy/.oh-my-zsh
ZSH_THEME="bureau"
plugins=(git)

source $ZSH/oh-my-zsh.sh
setxkbmap -option caps:swapescape
set -o vi
alias clipboard="xclip -selection c"
alias src="cd ~/src"

