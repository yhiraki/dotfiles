# zmodload zsh/zprof && zprof

# /etc/profile を無効化
setopt no_global_rcs

set -a

XDG_CONFIG_HOME="$HOME/.config"
XDG_CACHE_HOME="$HOME/.cache"

ZDOTDIR="$XDG_CONFIG_HOME/zsh"

SHELL=/bin/zsh

# PATHおよび各種検索パスの重複を自動排除する設定 (外部プロセス起動なしで高速)
typeset -U path PATH fpath manpath

PATH="/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/go/bin:/opt/X11/bin:$PATH"

GOPATH="$HOME"
PATH="$PATH:$GOPATH/bin"

RUSTPATH="$HOME/.cargo"
PATH="$PATH:$RUSTPATH/bin"

PYTHONUSERBASE="$HOME"

DOTDIR="$GOPATH/src/github.com/yhiraki/dotfiles"
PATH="$PATH:$DOTDIR/bin"

PATH="$PATH:$HOME/.local/bin:$PATH"
PATH="$PATH:/mnt/c/Windows/System32:$PATH"

# brew
[ -d /opt/homebrew ] && eval "$(/opt/homebrew/bin/brew shellenv)"
[ -d "${HOME}/.local/homebrew" ] && eval "$(${HOME}/.local/homebrew/bin/brew shellenv)"

command -v brew >/dev/null && {
  for i in {coreutils,gnu-sed,findutils,gnu-tar,grep,make}; do
    if [ -d "$HOMEBREW_PREFIX/opt/$i/libexec" ]; then
      PATH="$HOMEBREW_PREFIX/opt/$i/libexec/gnubin:$PATH"
      MANPATH="$HOMEBREW_PREFIX/opt/$i/libexec/gnuman:$MANPATH"
    fi
  done

  for i in {openssl,llvm,openjdk,mysql-client,libpq}; do
    if [ -d "$HOMEBREW_PREFIX/opt/$i/" ]; then
      PATH="$HOMEBREW_PREFIX/opt/$i/bin:$PATH"
    fi
  done

  # libgccjit
  LIBRARY_PATH="$HOMEBREW_PREFIX/opt/libgccjit/lib/gcc/12:${LIBRARY_PATH:-}"

  # option
  HOMEBREW_NO_AUTO_UPDATE=1
}

[ -d /Applications/Emacs.app/Contents/MacOS ] && {
  PATH="/Applications/Emacs.app/Contents/MacOS/bin:$PATH"
}

DENO_INSTALL="$HOME/.deno"
PATH="$DENO_INSTALL/bin:$PATH"

TERM=xterm-256color

LANG=ja_JP.UTF-8
LC_ALL=ja_JP.UTF-8

EDITOR=vim

HISTFILE="$HOME/.zsh_history"
HISTSIZE=10000
SAVEHIST=100000

ZPLUG_HOME="$HOME/.zplug"

PLANTUML_LIMIT_SIZE=8192

LESS='-R'
# LESSOPEN="| pygmentize %s"

# locate
LOCATE_PATH="$HOME/var/db/locate.database"

PIPENV_VENV_IN_PROJECT=true

PIP_REQUIRE_VIRTUALENV=true

REMOTE_WORKSPACE_FILE=/tmp/gce-workspace-ip

export PNPM_HOME="/home/yuta/.local/share/pnpm"
case ":$PATH:" in
  *":$PNPM_HOME:"*) ;;
  *) export PATH="$PNPM_HOME:$PATH" ;;
esac

set +a
