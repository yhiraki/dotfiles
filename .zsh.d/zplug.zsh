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
