# 自作関数の autoload 用パスを追加
fpath=($ZDOTDIR/functions $fpath)
autoload -Uz add-zsh-hook fsh fsql repo

if [[ "$INSIDE_EMACS" = 'vterm' ]] &&
  [[ -n ${EMACS_VTERM_PATH} ]] &&
  [[ -f ${EMACS_VTERM_PATH}/etc/emacs-vterm-zsh.sh ]]; then
  source ${EMACS_VTERM_PATH}/etc/emacs-vterm-zsh.sh
fi

# Viキーバインド
bindkey -v

# シェルオプション設定 (インタラクティブ用の設定は.zshrcで管理)
setopt hist_ignore_dups
setopt hist_ignore_all_dups
setopt hist_ignore_space
setopt EXTENDED_HISTORY

# 複数のzshを同時に使う時などhistoryファイルに上書きせず追加する
setopt append_history

# 同時に起動したzshの間でヒストリを共有する
setopt share_history

# ヒストリに保存するときに余分なスペースを削除する
setopt hist_reduce_blanks

# historyコマンドは履歴に登録しない
setopt hist_no_store

# 間違いを補完
setopt correct
setopt auto_param_keys
setopt list_packed

# 日本語ファイル名を表示可能にする
setopt print_eight_bit

setopt no_beep

# フローコントロールを無効にする
setopt no_flow_control

# リダイレクトのマルチ化
setopt multios

# ディレクトリ名だけで cd
setopt auto_cd

# cd + / cd - で過去にいたディレクトリに移動
setopt auto_pushd
setopt pushd_ignore_dups

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

# Tmux
{
  configure_tmux() {
    unset -f configure_tmux

    [ -z "$(tty)" ] && return
    [ -z "${TTY}" ] && return
    [ "${TERM_PROGRAM}" = "vscode" ] && return

    if ! command -v tmux >/dev/null; then
      return
    fi

    if [ -z "$INSIDE_EMACS" ] && [ -n "$TMUX_IGNORE" ]; then
      return
    fi

    if [[ -n "$TMUX" ]]; then
      function my_refresh_tmux_status() {
        tmux refresh-client -S
      }
      add-zsh-hook periodic my_refresh_tmux_status
      return
    fi

    local pj_root cwd session window envs

    pj_root=$(git rev-parse --show-toplevel)
    pj_root=${pj_root//./_}
    pj_name=$(echo $pj_root | cut -d/ -f6-8)
    cwd=${PWD//./_}
    session="${pj_name:-default}"
    window="${cwd/#${pj_root}/}"
    window="${window:-_}"

    if tmux has-session -t "${session}"; then
      if ! tmux has-session -t "${session}:${window}"; then
        tmux new-window -t "${session}" -n "${window}"
      fi
      exec tmux attach-session -t "${session}:${window}"
    fi

    exec tmux new-session -A -s "${session}" -n "${window}"
  }

  configure_tmux
}

# Aliases
{

  emacsclient-gui() {
    bash -c "emacsclient -c -a '' $1 &"
  }

  alias e='emacsclient -nw -a ""'
  alias ge=emacsclient-gui

  if [[ "$OSTYPE" == darwin* ]]; then
    alias emacs='/Applications/Emacs.app/Contents/MacOS/Emacs'
  fi

  alias ...='cd ../..'
  alias ....='cd ../../..'

  alias ls="ls -FG"

  alias -g F='| ff'
  alias -g O='| xargs open'
  alias -g C='| pbcopy'
  alias -g WP="| sed -e 's:^.*@://:' | tr '/' '\\\\'"

}

# Line Editor wrappers
{

  ignore-history() {
    BUFFER=" ${BUFFER}"
    CURSOR+=1
  }

  insert-buffer() {
    local arg
    arg="$1"
    RBUFFER="${arg}${RBUFFER}"
    CURSOR+=${#arg}
  }

  replace-buffer() {
    BUFFER="$1"
    CURSOR+=$#BUFFER
  }

}

# Fuzzy finder wrappers
{

  ff() {
    eval "${FF_CMD} ${FF_OPTIONS}"
  }

  ff-select-repo() {
    ghq list | ff |
      xargs -r -I{} echo "$(ghq root)/"{}
  }

  ff-branch-name() {
    git branch -a | ff | tail -c +3
  }

  ff-find-file() {
    local d="${1//~/$HOME}"
    find "${d}" 2>/dev/null |
      sed -e "s:^${d}/\?:./:" |
      ff |
      sed -e "s:^\.:${d}:"
  }

}

# Commands
{

  gitroot() {
    git rev-parse --show-toplevel
  }

  cdroot() {
    cd "$(gitroot)" || return
  }

  gpip() {
    PIP_REQUIRE_VIRTUALENV="" python3 -m pip "$@"
  }
}

# Zsh overrides
{

  zshaddhistory() {
    local line=${1%%$'\n'}
    local cmd=${line%% *}

    [[ ${cmd} != ls &&
      ${cmd} != cd ]]

  }

}

# Widgets
{

  widget-search-history-incremental() {
    replace-buffer "$(history -n 1 | tac | ff)"
    zle reset-prompt
  }
  zle -N widget-search-history-incremental

  widget-find-snippet() {
    local c
    c="｜"
    insert-buffer "$(
      sed -e '/^#/d' -e '/^$/d' -e 's/#|/'$c'/' ~/.snippets | ff |
        sed -e 's/ *'$c'.*$//'
    )"
  }
  zle -N widget-find-snippet

  widget-branch-name() {
    insert-buffer "$(ff-branch-name)"
    ignore-history
  }
  zle -N widget-branch-name

  widget-find-download-file() {
    insert-buffer "$(ff-find-file ~/Downloads)"
    ignore-history
  }
  zle -N widget-find-download-file

  widget-find-junkfile() {
    insert-buffer "$(ff-find-file ~/.cache/junkfile/)"
    ignore-history
  }
  zle -N widget-find-junkfile

  widget-find-repo-file() {
    insert-buffer "$(ff-find-file $(ff-select-repo))"
    ignore-history
  }
  zle -N widget-find-repo-file

  widget-find-current-repo-file() {
    insert-buffer "$(ff-find-file $(gitroot))"
    ignore-history
  }
  zle -N widget-find-current-repo-file

  widget-find-file() {
    insert-buffer "$(ff-find-file .)"
    ignore-history
  }
  zle -N widget-find-file

  widget-open-application() {
    local app apps
    apps=$(find \
      /Applications /System/Applications "${HOME}/Applications" \
      -maxdepth 2 -name '*.app')
    app=$(<<<$apps |
      sed -e 's:/.*/::' |
      sort |
      ff)
    <<<$apps | grep $app |
      xargs -r -I{} open -a '{}'
  }
  zle -N widget-open-application

  widget-select-widgets() {
    $(zle -l | grep -E '^widget-' | cut -d' ' -f1 | fzf)
  }
  zle -N widget-select-widgets

}

# Autoloads
{

  autoload -z edit-command-line
  zle -N edit-command-line

  autoload smart-insert-last-word
  zle -N insert-last-word smart-insert-last-word

}

# Bindings
{

  # \e → ESC
  # vi bind でも emacs bind を使用する
  # https://qiita.com/b4b4r07/items/8db0257d2e6f6b19ecb9
  bindkey -M viins '^?' backward-delete-char
  bindkey -M viins '^A' beginning-of-line
  bindkey -M viins '^B' backward-char
  #                '^C' SIGINT
  bindkey -M viins '^D' delete-char-or-list
  bindkey -M viins '^E' end-of-line
  bindkey -M viins '^F' forward-char
  bindkey -M viins '^G' send-break
  bindkey -M viins '^H' backward-delete-char
  #                '^I' expand-and-complete (TAB)
  #                '^J' accept-line (RETURN)
  bindkey -M viins '^K' kill-line
  #                '^L' clear-screen
  #                '^M' accept-line (RETURN)
  bindkey -M viins '^N' down-line-or-history
  #                '^O' nil
  bindkey -M viins '^P' up-line-or-history
  #                '^Q' vi-quoted-insert
  bindkey -M viins '^R' widget-search-history-incremental
  #                '^S' nil
  #                '^T' nil
  bindkey -M viins '^U' backward-kill-line
  #                '^V' vi-quoted-insert
  bindkey -M viins '^W' backward-kill-word
  #                '^X' prefix
  bindkey -M viins '^Y' yank
  #                '^X' prefix
  #                '^Z' suspend

  # zsh-autosuggestions
  bindkey '^ ' autosuggest-accept

  # find files
  bindkey -M viins '^Xfd' widget-find-download-file
  bindkey -M viins '^Xff' widget-find-file
  bindkey -M viins '^Xfj' widget-find-junkfile

  # git
  bindkey -M viins '^Xgb' widget-branch-name
  bindkey -M viins '^Xgf' widget-find-current-repo-file
  bindkey -M viins '^Xgg' widget-find-repo-file

  # autoloads
  bindkey -M viins "^Xe" edit-command-line
  bindkey -M viins "^Xl" insert-last-word

  # open app
  bindkey -M viins "^X^O" widget-open-application

  # snippet
  bindkey -M viins '^X^k' widget-find-snippet

  # all widgets
  bindkey -M viins '^X^X' widget-select-widgets

}

# Load plugins
{

  load_plugins() {
    command -v git >/dev/null || return

    plugins_repo=(
      github.com/paulirish/git-open
      github.com/zsh-users/zsh-autosuggestions
      github.com/zsh-users/zsh-completions
      github.com/zdharma-continuum/fast-syntax-highlighting
      github.com/yhiraki/zsh-simple-prompt
    )

    root="$ZDOTDIR/plugin/repos"
    local d
    for p in "${plugins_repo[@]}"; do
      d="$root/$p"
      [[ ! -d "$d" ]] && git clone "https://$p" "$d"
      source $d/*.plugin.zsh
    done
  }

  load_plugins
  unset -f load_plugins

}

# Fzf / Fuzzy Finder configurations (Moved from .zshenv)
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
      (cd $1 && find . -maxdepth 1) | cut -d / -f 2-
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

    command -v ghq >/dev/null || return

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
  FZF_DEFAULT_OPTS="\
--no-sort \
-e --ansi --select-1 --exit-0 \
--bind=ctrl-k:kill-line \
--preview='$FZF_PREVIEW_CMD'"

}

FF_CMD='fzf'
FF_OPTIONS="$FZF_DEFAULT_OPTS"

ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=242'

PANE_STATUS_STYLE_GIT_CWD='fg=#bfbfbf'
PANE_STATUS_STYLE_GIT_STATUS_ICONS='fg=red'
PANE_STATUS_ICON_PYTHON=' '
PANE_STATUS_ICON_GITHUB=' '
PANE_STATUS_ICON_BITBUCKET=' '
PANE_STATUS_ICON_BRANCH=''

SPL_PROMPT_NOTIFY_TIME_MIN=10000

# Direnv setup
command -v direnv >/dev/null &&
  eval "$(direnv hook zsh)"

# asdf setup
[ -d "$HOME/.asdf" ] && {
  . "$HOME/.asdf/asdf.sh"
  fpath=(${ASDF_DIR}/completions $fpath)
}

# mise setup
command -v mise >/dev/null && {
  eval "$(mise activate zsh)"
}

# 重複パスを自動的に排除するzshの組み込み設定 (外部プロセスを起動しないため超高速)
typeset -U path PATH fpath manpath

# 補完ダンプファイルの保存先を XDG Cache Directory 準拠へ変更し、ホームディレクトリを汚さないようにします
local zcomp_dir="${XDG_CACHE_HOME:-$HOME/.cache}/zsh"
[[ ! -d "$zcomp_dir" ]] && mkdir -p "$zcomp_dir"
local zcomp_file="$zcomp_dir/zcompdump"

fpath=($ZDOTDIR/completion $fpath)
fpath=($HOME/.zfunc $fpath)

# 補完関数の初期化 (1日以内のキャッシュがあればセキュリティチェックをスキップして高速起動)
autoload -Uz compinit
if [[ -s "$zcomp_file" ]]; then
  # 24時間以内に更新されたキャッシュがあれば -C を指定して高速ロード
  local -a dump_files
  dump_files=("$zcomp_file"(N-m-1))
  if (( ${#dump_files} )); then
    compinit -C -d "$zcomp_file"
  else
    compinit -i -d "$zcomp_file"
  fi
else
  compinit -i -d "$zcomp_file"
fi

# 補完ダンプが更新されていたら、バックグラウンドで zcompile してバイトコード化 (.zwc化してさらに高速化)
if [[ -s "$zcomp_file" ]]; then
  if [[ ! -s "${zcomp_file}.zwc" || "$zcomp_file" -nt "${zcomp_file}.zwc" ]]; then
    zcompile "$zcomp_file"
  fi
fi

# emacs vterm
{

  if [[ "$INSIDE_EMACS" = 'vterm' &&
    -n ${EMACS_VTERM_PATH} &&
    -f ${EMACS_VTERM_PATH}/etc/emacs-vterm-zsh.sh ]]; then
    source ${EMACS_VTERM_PATH}/etc/emacs-vterm-zsh.sh
    # Initialize TITLE
    print -Pn "\e]2;%m:%2~\a"
  fi

}

# gcloud
[ -d "$HOME/.local/google-cloud-sdk" ] && {

  source "$HOME/.local/google-cloud-sdk/path.zsh.inc"
  source "$HOME/.local/google-cloud-sdk/completion.zsh.inc"

}

# aws
[ -d "$HOME/.local/aws-cli" ] && {

  complete -C "$HOME/.local/aws-cli/aws_completer" aws

}

# uncomment to profile
# zmodload zsh/zprof && zprof

# Make status code '0'
echo .zshrc loaded 1>&2

# Local .zshrc
[[ -f "$HOME/.zshrc.local" ]] && source "$HOME/.zshrc.local"
