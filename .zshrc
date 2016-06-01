# 設定はここを参考にした
# http://qiita.com/uasi/items/c4288dd835a65eb9d70

# pure
autoload -U promptinit && promptinit

# 固有設定の読込
[ -f ~/.zshrc.local ] && source ~/.zshrc.local

# zplug
export ZPLUG_HOME=$HOME/.zplug
if [ ! -d $ZPLUG_HOME ]; then
  git clone https://github.com/zplug/zplug $ZPLUG_HOME
fi
source $ZPLUG_HOME/init.zsh

zplug "junegunn/fzf", as:command, use:bin/fzf-tmux
zplug "zsh-users/zsh-syntax-highlighting", nice:10
zplug "mollifier/anyframe"
zplug "zsh-users/zsh-completions"
zplug "mafredri/zsh-async"
zplug "sindresorhus/pure"

# check installed
if ! zplug check --verbose; then
    # printf "Install? [y/N]: "
    # if read -q; then
        echo; zplug install
    # fi
fi

# load plugins
zplug load

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# 分割されたzshrc
ZSHHOME="${ZDOTDIR}/.zsh.d"

case ${OSTYPE} in
  darwin*)
    source $ZSHHOME/.zshrc.darwin
    ;;
  linux*)
    source $ZSHHOME/.zshrc.linux
    ;;
  msys*)
    source $ZSHHOME/.zshrc.msys
    ;;
esac

if which anyframe-widget-select-widget > /dev/null; then
  source $ZSHHOME/.zshrc.anyframe
fi

# cal 今日の日付に色を付ける
alias cal='cal | grep -C6 --color $(date +%d)'

repos() {
  local dir
  dir=$(ghq list > /dev/null | fzf-tmux) &&
    cd $(ghq root)/$dir
}

