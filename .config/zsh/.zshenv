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

# autoload -U promptinit && promptinit
# autoload -U compinit && compinit -C

export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export ZDOTDIR="$XDG_CONFIG_HOME/zsh"

if [ -x /usr/libexec/path_helper ]; then
  eval `/usr/libexec/path_helper -s`
fi

export GOPATH="$HOME"
export PATH="$PATH:$GOPATH/bin"

export RUSTPATH="$HOME/.cargo"
export PATH="$PATH:$RUSTPATH/bin"

export DOTDIR="$GOPATH/src/github.com/yhiraki/dotfiles"
export PATH="$PATH:$DOTDIR/bin"

# gcloud
# export CLOUDSDK_PYTHON_SITEPACKAGES=1
# export PATH=$PATH:$HOME/bin/google-cloud-sdk/bin

export PATH="$HOME/.anyenv/bin:$PATH"

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
