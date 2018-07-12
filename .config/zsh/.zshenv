# zmodload zsh/zprof && zprof

export XDG_CONFIG_HOME=$HOME/.config
export XDG_CACHE_HOME=$HOME/.cache
export ZDOTDIR=$XDG_CONFIG_HOME/zsh

export TERM='xterm-256color'

# for alacritty
export PATH=/usr/local/bin:$PATH

export LANG=ja_JP.UTF-8

export EDITOR="emacsclient -nw"
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

export FZF_CMD='fzf-tmux'
export FZF_PREVIEW_CMD=" cat | head -500"

export LESS='-R'
# export LESSOPEN="| pygmentize %s"

# locate
export LOCATE_PATH=$HOME/var/db/locate.database

# gcloud
export CLOUDSDK_PYTHON_SITEPACKAGES=1
export PATH=$PATH:$HOME/bin/google-cloud-sdk/bin

ANYENV_ROOT=$HOME/.anyenv

export PIPENV_VENV_IN_PROJECT=true

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
setopt extended_glob

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

# zmvの設定
# http://mollifier.hatenablog.com/entry/20101227/
autoload -Uz zmv
alias zmv='noglob zmv -W'

# 色を使う
setopt prompt_subst

autoload -U promptinit && promptinit
