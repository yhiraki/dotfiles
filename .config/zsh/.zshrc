[ -f ~/.zshrc.local ] && source ~/.zshrc.local

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

source $ZDOTDIR/zplug.zsh

# direnv setup
eval "$(direnv hook zsh)"
