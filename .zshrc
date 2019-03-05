# The following lines were added by compinstall

zstyle ':completion:*' completer _expand _complete _ignored _correct _approximate
zstyle ':completion:*' matcher-list '' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' 'r:|[._-]=** r:|=**' 'l:|=* r:|=*'
zstyle :compinstall filename '/home/kyle/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall
# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=10000000
SAVEHIST=100000000
setopt appendhistory extendedglob notify
unsetopt autocd beep nomatch
bindkey -e
# End of lines configured by zsh-newuser-install
PS1="%~
(%?) <`hostname -s`:`whoami`> $ "

source $HOME/.profile
eval "$(direnv hook zsh)"


[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
