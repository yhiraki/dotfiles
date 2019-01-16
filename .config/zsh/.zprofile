export TERM=xterm-256color

export LANG=ja_JP.UTF-8
export LC_ALL=ja_JP.UTF-8

export EDITOR=vim

export HISTFILE="$HOME/.zsh_history"
export HISTSIZE=10000
export SAVEHIST=100000

export FZF_DEFAULT_OPTS="-e --ansi --select-1 --exit-0"
export ZPLUG_HOME="$HOME/.zplug"

export PLANTUML_LIMIT_SIZE=8192

export FZF_CMD='fzf-tmux'
export FZF_PREVIEW_CMD=" cat | head -500"

export LESS='-R'
# export LESSOPEN="| pygmentize %s"

# locate
export LOCATE_PATH="$HOME/var/db/locate.database"

export PIPENV_VENV_IN_PROJECT=true

# anyenv
eval "$(anyenv init -)"

# gcloud
# source $HOME/bin/google-cloud-sdk/path.zsh.inc
