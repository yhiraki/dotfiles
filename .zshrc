# http://news.mynavi.jp/column/zsh/002/
#
# set prompt
#

# 日本語の設定
export LANG=ja_JP.UTF-8

# ヒストリの設定
# http://qiita.com/syui/items/c1a1567b2b76051f50c4

# 履歴ファイルの保存先
export HISTFILE=${HOME}/.zsh_history

# メモリに保存される履歴の件数
export HISTSIZE=10000

# 履歴ファイルに保存される履歴の件数
export SAVEHIST=100000

# 重複を記録しない
setopt hist_ignore_dups

# 開始と終了を記録
setopt EXTENDED_HISTORY

# http://news.mynavi.jp/column/zsh/005/
# 間違ったコマンドを補完
setopt correct

# lsの候補を詰めて表示
setopt list_packed

# Beep音を無効化
setopt no_beep

# 対応カッコなどを自動的に保管
setopt auto_param_keys

# http://news.mynavi.jp/column/zsh/006/
# 先方予測機能
# autoload predict-on; predict-on

# リダイレクトのマルチ化
# http://news.mynavi.jp/column/zsh/007/
setopt multios

# 設定はここを参考にした
# http://qiita.com/uasi/items/c4288dd835a65eb9d70

# Vi ライクな操作が好みであれば `bindkey -v` とする
bindkey -v

# 自動補完を有効にする
# コマンドの引数やパス名を途中まで入力して <Tab> を押すといい感じに補完してくれる
# 例： `cd path/to/<Tab>`, `ls -<Tab>`
autoload -U compinit; compinit -u
compinit -C

# pure
autoload -U promptinit && promptinit

# 入力したコマンドが存在せず、かつディレクトリ名と一致するなら、ディレクトリに cd する
# 例： /usr/bin と入力すると /usr/bin ディレクトリに移動
setopt auto_cd

# ↑を設定すると、 .. とだけ入力したら1つ上のディレクトリに移動できるので……
# 2つ上、3つ上にも移動できるようにする
alias ...='cd ../..'
alias ....='cd ../../..'

# "~hoge" が特定のパス名に展開されるようにする（ブックマークのようなもの）
# 例： cd ~hoge と入力すると /long/path/to/hogehoge ディレクトリに移動
# hash -d hoge=/long/path/to/hogehoge

# cd した先のディレクトリをディレクトリスタックに追加する
# ディレクトリスタックとは今までに行ったディレクトリの履歴のこと
# `cd +<Tab>` でディレクトリの履歴が表示され、そこに移動できる
setopt auto_pushd

# pushd したとき、ディレクトリがすでにスタックに含まれていればスタックに追加しない
setopt pushd_ignore_dups

# 拡張 glob を有効にする
# glob とはパス名にマッチするワイルドカードパターンのこと
# （たとえば `mv hoge.* ~/dir` における "*"）
# 拡張 glob を有効にすると # ~ ^ もパターンとして扱われる
# どういう意味を持つかは `man zshexpn` の FILENAME GENERATION を参照
setopt extended_glob

# 入力したコマンドがすでにコマンド履歴に含まれる場合、履歴から古いほうのコマンドを削除する
# コマンド履歴とは今まで入力したコマンドの一覧のことで、上下キーでたどれる
setopt hist_ignore_all_dups

# コマンドがスペースで始まる場合、コマンド履歴に追加しない
# 例： <Space>echo hello と入力
setopt hist_ignore_space

# <Tab> でパス名の補完候補を表示したあと、
# 続けて <Tab> を押すと候補からパス名を選択できるようになる
# 候補を選ぶには <Tab> か Ctrl-N,B,F,P
zstyle ':completion:*:default' menu select=1

# 単語の一部として扱われる文字のセットを指定する
# ここではデフォルトのセットから / を抜いたものとする
# こうすると、 Ctrl-W でカーソル前の1単語を削除したとき、 / までで削除が止まる
WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'

## このサイトを参考にした
# http://qiita.com/KENJU/items/828f5319ca29b9928f70

# 色を使う
setopt prompt_subst

# 外部ファイルの読み込み設定
# http://news.mynavi.jp/column/zsh/006/
[ -f ~/.zshrc.local ] && source ~/.zshrc.local

# https://gist.github.com/mollifier/4979906
# 補完で小文字でも大文字にマッチさせる
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

# zmvの設定
# http://mollifier.hatenablog.com/entry/20101227/
autoload -Uz zmv
alias zmv='noglob zmv -W'

# zplug
export ZPLUG_HOME=$HOME/.zplug
if [ ! -d $ZPLUG_HOME ]; then
  git clone https://github.com/zplug/zplug $ZPLUG_HOME
fi
source $ZPLUG_HOME/init.zsh

zplug "junegunn/fzf", as:command, use:bin/fzf-tmux
zplug "zsh-users/zsh-syntax-highlighting", nice:10
zplug "mollifier/anyframe"
zplug "zsh-users/zsh-completions"
zplug "mafredri/zsh-async"
zplug "sindresorhus/pure"

# check installed
if ! zplug check --verbose; then
    # printf "Install? [y/N]: "
    # if read -q; then
        echo; zplug install
    # fi
fi

# load plugins
zplug load

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# 分割されたzshrc
ZSHHOME="${ZDOTDIR}/.zsh.d"

case ${OSTYPE} in
  darwin*)
    source $ZSHHOME/.zshrc.darwin
    ;;
  linux*)
    source $ZSHHOME/.zshrc.linux
    ;;
  msys*)
    source $ZSHHOME/.zshrc.msys
    ;;
esac

if which anyframe-widget-select-widget > /dev/null; then
  source $ZSHHOME/.zshrc.anyframe
fi

# cal 今日の日付に色を付ける
alias cal='cal | grep -C6 --color $(date +%d)'

# gopath
export GOPATH=$HOME
export PATH=$PATH:$GOPATH/bin

repos() {
  local dir
  dir=$(ghq list > /dev/null | fzf-tmux) &&
    cd $(ghq root)/$dir
}

