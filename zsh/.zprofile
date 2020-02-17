export SHELL=/bin/zsh

export PATH="/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/go/bin:/opt/X11/bin"

export GOPATH="$HOME"
export PATH="$PATH:$GOPATH/bin"

export RUSTPATH="$HOME/.cargo"
export PATH="$PATH:$RUSTPATH/bin"

export DOTDIR="$GOPATH/src/github.com/yhiraki/dotfiles"
export PATH="$PATH:$DOTDIR/bin"

export PATH="/usr/local/opt/mysql-client/bin:$PATH"

for i in {coreutils,gnu-sed,findutils,gnu-tar,grep}
do
  if [ -d /usr/local/opt/$i/libexec ]
  then
    export PATH="/usr/local/opt/$i/libexec/gnubin:$PATH"
    export MANPATH="/usr/local/opt/$i/libexec/gnuman:$MANPATH"
  fi
done

if [ -d /usr/local/opt/openssl/ ]
then
  export PATH="/usr/local/opt/openssl/bin:$PATH"
fi

if [ -d /usr/local/opt/llvm/ ]
then
  export PATH="/usr/local/opt/llvm/bin:$PATH"
fi

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

# Fuzzy finder
# export FF_CMD='gof'
# export FF_OPTIONS='-f -i "^(\\.git|\\.hg|\\.svn|_darcs|\\.bzr|\\.pyc|\\.venv)$"'
export FF_CMD='fzf'

# anyenv
# eval "$(anyenv init -)"

# gcloud
# source $HOME/bin/google-cloud-sdk/path.zsh.inc

export SSH_AGENT_RC=/tmp/ssh-agent-rc

if [ ! -f $SSH_AGENT_RC ]
then
  ssh-agent > $SSH_AGENT_RC
  ssh-add
fi

. $SSH_AGENT_RC

if ! kill -s 0 $SSH_AGENT_PID
then
  ssh-agent > $SSH_AGENT_RC
  ssh-add
  . $SSH_AGENT_RC
fi
