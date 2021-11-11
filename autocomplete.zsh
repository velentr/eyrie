autoload -U compinit && compinit

setopt completealiases

eval `dircolors -b`
zstyle ':completion:*' list-colors '${(s.:.)LS_COLORS}'
zstyle ':completion:*' menu select eval "$(dircolors -b)"
