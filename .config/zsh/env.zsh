export TERM='xterm-256color'

export LANG=ja_JP.UTF-8

export EDITOR=nvim
# export EDITOR=vim

export HISTFILE=${HOME}/.zsh_history
export HISTSIZE=10000
export SAVEHIST=100000

export GOPATH=$HOME
export PATH=$PATH:$GOPATH/bin

export RUSTPATH=$HOME/.cargo
export PATH=$PATH:$RUSTPATH/bin

export DOTDIR=$GOPATH/src/github.com/coffexpr/dotfiles

export FZF_DEFAULT_OPTS="--ansi --select-1 --exit-0"
export ZPLUG_HOME=$HOME/.zplug

export PLANTUML_LIMIT_SIZE=8192

export FZF_CMD='fzf-tmux'
