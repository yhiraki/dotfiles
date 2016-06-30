# 固有設定の読込
[ -f ~/.zshrc.local ] && source ~/.zshrc.local

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

source $ZDOTDIR/zplug.zsh
source $ZDOTDIR/func.zsh

if which anyframe-widget-select-widget > /dev/null; then
  source $ZDOTDIR/anyframe.zsh
fi

PATH=$PATH:$DOTDIR/bin

source $ZDOTDIR/alias.zsh

case ${OSTYPE} in
  darwin*)
    source $ZDOTDIR/darwin.zsh
    ;;
  linux*)
    source $ZDOTDIR/linux.zsh
    ;;
  msys*)
    source $ZDOTDIR/msys.zsh
    ;;
esac

# direnv setup
eval "$(direnv hook zsh)"
