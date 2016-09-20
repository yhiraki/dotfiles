export DOTDIR=$HOME/src/github.com/awaman/dotfiles
source $DOTDIR/zsh/env.zsh

# history 関連
setopt hist_ignore_dups
setopt hist_ignore_all_dups
setopt hist_ignore_space
setopt EXTENDED_HISTORY
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

setopt no_beep

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

# 単語の一部として扱われる文字
WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'

# zmvの設定
# http://mollifier.hatenablog.com/entry/20101227/
autoload -Uz zmv
alias zmv='noglob zmv -W'

# 色を使う
setopt prompt_subst

autoload -U promptinit && promptinit
