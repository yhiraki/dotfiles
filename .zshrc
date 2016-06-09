# 固有設定の読込
[ -f ~/.zshrc.local ] && source ~/.zshrc.local

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# 分割されたzshrc
ZSHHOME="${ZDOTDIR}/.zsh.d"

source $ZSHHOME/zplug.zsh
source $ZSHHOME/func.zsh

if which anyframe-widget-select-widget > /dev/null; then
  source $ZSHHOME/anyframe.zsh
fi

case ${OSTYPE} in
  darwin*)
    source $ZSHHOME/darwin.zsh
    ;;
  linux*)
    source $ZSHHOME/linux.zsh
    ;;
  msys*)
    source $ZSHHOME/msys.zsh
    ;;
esac

# cal 今日の日付に色を付ける
alias cal='cal | grep -C6 --color $(date +%d)'

# direnv setup
eval "$(direnv hook zsh)"
