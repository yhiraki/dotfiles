export TERM='xterm-256color'

# for alacritty
export PATH=/usr/local/bin:$PATH

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
export PATH=$PATH:$DOTDIR/bin

export FZF_DEFAULT_OPTS="-e --ansi --select-1 --exit-0"
export ZPLUG_HOME=$HOME/.zplug

export PLANTUML_LIMIT_SIZE=8192

if which pygmentize > /dev/null
then
  highlight_cmd='pygmentize'
elif which /usr/bin/src-hilite-lesspipe.sh > /dev/null
then
  highlight_cmd='/usr/bin/src-hilite-lesspipe.sh'
else
  highlight_cmd='cat'
fi

export FZF_CMD='fzf-tmux'
export FZF_PREVIEW_CMD='pygmentize {} | head -500'

export CLOUDSDK_PYTHON_SITEPACKAGES=1

# requires source-highlight
export LESS='-R'
export LESSOPEN='| pygmentize %s'

# locate
export LOCATE_PATH=$HOME/var/db/locate.database
