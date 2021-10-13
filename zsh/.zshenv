# zmodload zsh/zprof && zprof

# /etc/profile を無効化
setopt no_global_rcs

setopt hist_ignore_dups
setopt hist_ignore_all_dups
setopt hist_ignore_space
setopt EXTENDED_HISTORY

# 複数のzshを同時に使う時などhistoryファイルに上書きせず追加する
setopt append_history

#同時に起動したzshの間でヒストリを共有する
setopt share_history

# ヒストリに保存するときに余分なスペースを削除する
setopt hist_reduce_blanks

# historyコマンドは履歴に登録しない
setopt hist_no_store

# 間違いを補完
setopt correct
setopt auto_param_keys
setopt list_packed
# 先方予測機能
# autoload predict-on; predict-on

# 日本語ファイル名を表示可能にする
setopt print_eight_bit

setopt no_beep

# フローコントロールを無効にする
setopt no_flow_control

# リダイレクトのマルチ化
setopt multios

# Viキーバインド
bindkey -v

# ディレクトリ名だけで cd
setopt auto_cd

# cd + / cd - で過去にいたディレクトリに移動
setopt auto_pushd
setopt pushd_ignore_dups

# 拡張 glob
# setopt extended_glob

# 配列を0オリジンにする # zshが起動できなくなるので無効化
# setopt ksharrays

# '#'以降をコメントとして扱う
setopt interactive_comments

# <Tab> でパス名を選択
# 候補を選ぶには <Tab> か Ctrl-N,B,F,P
zstyle ':completion:*:default' menu select=1

# 補完で小文字でも大文字にマッチさせる
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

# sudo の後ろでコマンド名を補完する
zstyle ':completion:*:sudo:*' command-path /usr/local/sbin /usr/local/bin \
  /usr/sbin /usr/bin /sbin /bin /usr/X11R6/bin

# 単語の一部として扱われる文字
WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'

# 色を使う
setopt prompt_subst

# autoload -U promptinit && promptinit
autoload -U compinit && compinit -C

set -a

XDG_CONFIG_HOME="$HOME/.config"
XDG_CACHE_HOME="$HOME/.cache"

ZDOTDIR="$XDG_CONFIG_HOME/zsh"

SHELL=/bin/zsh

PATH="/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/go/bin:/opt/X11/bin"

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
  for i in {coreutils,gnu-sed,findutils,gnu-tar,grep}; do
    if [ -d "$HOMEBREW_PREFIX/opt/$i/libexec" ]; then
      PATH="$HOMEBREW_PREFIX/opt/$i/libexec/gnubin:$PATH"
      MANPATH="$HOMEBREW_PREFIX/opt/$i/libexec/gnuman:$MANPATH"
    fi
  done

  for i in {opensl,llvm,openjdk,mysql-client}; do
    if [ -d "$HOMEBREW_PREFIX/opt/$i/" ]; then
      PATH="$HOMEBREW_PREFIX/opt/$i/bin:$PATH"
    fi
  done

  # libgccjit
  LIBRARY_PATH="$HOMEBREW_PREFIX/opt/libgccjit/lib/gcc/11"
}

TERM=xterm-256color

LANG=ja_JP.UTF-8
LC_ALL=ja_JP.UTF-8

EDITOR=vim

HISTFILE="$HOME/.zsh_history"
HISTSIZE=10000
SAVEHIST=100000

FZF_DEFAULT_OPTS="-e --ansi --select-1 --exit-0"
ZPLUG_HOME="$HOME/.zplug"

PLANTUML_LIMIT_SIZE=8192

LESS='-R'
# LESSOPEN="| pygmentize %s"

# locate
LOCATE_PATH="$HOME/var/db/locate.database"

PIPENV_VENV_IN_PROJECT=true

# fzf
command -v fzf >/dev/null && {

  __fzf_preview_func() {
    # is File
    if [ -f "$1" ]; then
      file "$1"
      echo '-----'
      cat "$1" | head -100
      return
    fi

    # is Directory
    if [ -d "$1" ]; then
      tree "$1" | head -100
      return
    fi

    # is Command
    local cmd
    cmd=$(cut -d' ' -f1 <<<"$1")
    if type "$cmd" >/dev/null; then
      if man "$cmd"; then
        return
      fi
      type -a "$cmd"
      return
    fi

    # is Git Project
    local srcd readme
    srcd="$(ghq root)/$1"
    if [ -d "$srcd" ]; then
      readme=$(find "$srcd" -maxdepth 1 -name 'README*' | head -1)
      # has README
      if [ -f "$readme" ]; then
        cat "$readme"
        return
      fi
      tree "$srcd" | head -100
      return
    fi
  }

  FZF_DEFAULT_CMD='fd --type f'
  FZF_PREVIEW_CMD='__fzf_preview_func {}'
  FZF_DEFAULT_OPTS=" \
--no-sort \
--bind=ctrl-k:kill-line \
--preview='$FZF_PREVIEW_CMD'"

}

# Fuzzy finder
# FF_CMD='gof'
# FF_OPTIONS='-f -i "^(\\.git|\\.hg|\\.svn|_darcs|\\.bzr|\\.pyc|\\.venv)$"'
FF_CMD='fzf'
FF_OPTIONS="$FZF_DEFAULT_OPTS"

ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=242'

PY_TMUX_PANE_OPTIONS__GIT_CWD='fg=#bfbfbf'
PY_TMUX_PANE_OPTIONS__GIT_STATUS_ICONS='fg=red'
PY_TMUX_PANE_ICON__PYTHON=' '
PY_TMUX_PANE_ICON__GITHUB=' '
PY_TMUX_PANE_ICON__BITBUCKET=' '
PY_TMUX_PANE_ICON__BRANCH=''

SPL_PROMPT_NOTIFY_TIME_MIN=10000

set +a
