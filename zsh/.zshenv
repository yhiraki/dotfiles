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

PATH="/usr/local/opt/mysql-client/bin:$PATH"

PATH="$PATH:$HOME/.local/bin:$PATH"
PATH="$PATH:/mnt/c/Windows/System32:$PATH"

for i in {coreutils,gnu-sed,findutils,gnu-tar,grep}
do
  if [ -d /usr/local/opt/$i/libexec ]
  then
    export PATH="/usr/local/opt/$i/libexec/gnubin:$PATH"
    export MANPATH="/usr/local/opt/$i/libexec/gnuman:$MANPATH"
  fi
done

if [ -d /usr/local/opt/openssl/ ]; then
  PATH="/usr/local/opt/openssl/bin:$PATH"
fi

if [ -d /usr/local/opt/llvm/ ]; then
  PATH="/usr/local/opt/llvm/bin:$PATH"
fi

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

FZF_CMD='fzf-tmux'
FZF_PREVIEW_CMD=" cat | head -500"

LESS='-R'
# LESSOPEN="| pygmentize %s"

# locate
LOCATE_PATH="$HOME/var/db/locate.database"

PIPENV_VENV_IN_PROJECT=true

# Fuzzy finder
# FF_CMD='gof'
# FF_OPTIONS='-f -i "^(\\.git|\\.hg|\\.svn|_darcs|\\.bzr|\\.pyc|\\.venv)$"'
FF_CMD='fzf'
FF_OPTIONS='--no-sort --bind=ctrl-k:kill-line'

ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=242'

PY_TMUX_PANE_OPTIONS__GIT_CWD='fg=#bfbfbf'
PY_TMUX_PANE_OPTIONS__GIT_STATUS_ICONS='fg=red'
PY_TMUX_PANE_ICON__PYTHON=' '
PY_TMUX_PANE_ICON__GITHUB=' '
PY_TMUX_PANE_ICON__BITBUCKET=' '
PY_TMUX_PANE_ICON__BRANCH=''

SPL_PROMPT_NOTIFY_TIME_MIN=10000

# libgccjit
LIBRARY_PATH="$(brew --prefix libgccjit)/lib/gcc/10"
