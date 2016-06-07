export ZDOTDIR=$HOME/src/github.com/awa-manju/dotfiles

export XDG_CONFIG_HOME=$HOME/.config
export XDG_CACHE_HOME=$HOME/.cache

export LANG=ja_JP.UTF-8

export EDITOR=nvim
# export EDITOR=vim

# history 関連
export HISTFILE=${HOME}/.zsh_history
export HISTSIZE=10000
export SAVEHIST=100000
setopt hist_ignore_dups
setopt hist_ignore_all_dups
setopt hist_ignore_space
setopt EXTENDED_HISTORY

# 間違いを補完
setopt correct
setopt auto_param_keys
setopt list_packed
# 先方予測機能
# autoload predict-on; predict-on
# 自動補完を有効化
autoload -U compinit; compinit -u
compinit -C

setopt no_beep

# リダイレクトのマルチ化
setopt multios

export GOPATH=$HOME
export PATH=$PATH:$GOPATH/bin

# Viキーバインド
bindkey -v

# ディレクトリ名だけで cd
setopt auto_cd

alias ...='cd ../..'
alias ....='cd ../../..'

# cd + / cd - で過去にいたディレクトリに移動
setopt auto_pushd
setopt pushd_ignore_dups

# 拡張 glob
setopt extended_glob

# <Tab> でパス名を選択
# 候補を選ぶには <Tab> か Ctrl-N,B,F,P
zstyle ':completion:*:default' menu select=1

# 補完で小文字でも大文字にマッチさせる
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

# 単語の一部として扱われる文字
WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'

# zmvの設定
# http://mollifier.hatenablog.com/entry/20101227/
autoload -Uz zmv
alias zmv='noglob zmv -W'

# 色を使う
setopt prompt_subst
