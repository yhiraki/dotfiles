if [ ! -d $ZPLUG_HOME ]; then
  git clone https://github.com/zplug/zplug $ZPLUG_HOME
fi
source $ZPLUG_HOME/init.zsh

zplug "junegunn/fzf", as:command, use:bin/fzf-tmux
zplug "zsh-users/zsh-syntax-highlighting", nice:10
zplug "mollifier/anyframe"
zplug "zsh-users/zsh-completions"
zplug "felixr/docker-zsh-completion"
zplug "mafredri/zsh-async"
zplug "sindresorhus/pure"
zplug "aws/aws-cli", use:bin/aws_zsh_completer.sh, on:"zsh-users/zsh-completions", nice:10

zplug "$ZDOTDIR/func.zsh", from:local
zplug "$ZDOTDIR/anyframe.zsh", from:local, on:"mollifier/anyframe"
zplug "$ZDOTDIR/alias.zsh", from:local

case ${OSTYPE} in
  darwin*)
    zplug "$ZDOTDIR/darwin.zsh", from:local
    ;;
  linux*)
    zplug "$ZDOTDIR/linux.zsh", from:local
    ;;
  msys*)
    zplug "$ZDOTDIR/msys.zsh", from:local
    ;;
esac

# check installed
if ! zplug check --verbose; then
    # printf "Install? [y/N]: "
    # if read -q; then
        echo; zplug install
    # fi
fi

zplug load

# 自動補完を有効化
autoload -U compinit; compinit -u
compinit -C
