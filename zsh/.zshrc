# Local .zshrc
[[ -f "$HOME/.zshrc.local" ]] && source "$HOME/.zshrc.local"

autoload -Uz add-zsh-hook

# Tmux
{
  configure_tmux() {
    if ! command -v tmux >/dev/null ||
      [ -n "$INSIDE_EMACS" ] ||
      [ -n "$TMUX_IGNORE" ]; then
      return
    fi

    if [[ -n "$TMUX" ]]; then
      function my_refresh_tmux_status() {
        tmux refresh-client -S
      }
      add-zsh-hook periodic my_refresh_tmux_status
      return
    fi

    local TMUX_DEFAULT_NAME=default

    if tmux has -t "${TMUX_DEFAULT_NAME}"; then
      exec tmux attach -t "${TMUX_DEFAULT_NAME}"
      return
    fi

    exec tmux new -s "${TMUX_DEFAULT_NAME}"
  }
  [ -n "${TTY}" ] || [ -n "${tty}" ] &&
    configure_tmux
  unset -f configure_tmux
}

# Aliases
{

  emacsclient-gui() {
    bash -c "emacsclient -c -a '' $1 &"
  }

  alias e='emacsclient -nw -a ""'
  alias ge=emacsclient-gui

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

  fsh() {
    local host
    host=$(grep ~/.ssh/config -i -e '^host' |
      sed -e 's/host //i' \
        -e '/*/d' |
      ff) || return
    ssh "${host}"
  }

  fsql() {
    local host
    host=$(sed -E 's/:[^:]+$//' ~/.pgpass | ff |
      sed -e 's/^/-h /' \
        -e 's/:/ -p /' \
        -e 's/:/ -d /' \
        -e 's/:/ -U /') || return
    psql "${host}"
  }

  repo() {
    cd "$(ff-select-repo)" || return
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

# Direnv setup
command -v direnv >/dev/null &&
  eval "$(direnv hook zsh)"

# Path sort by string length
export PATH=$(echo "$PATH" |
  tr : '\n' |
  awk '{print length(), $0}' |
  sort -nr |
  cut -d ' ' -f 2 |
  uniq |
  tr '\n' :)

command -v zprof >/dev/null &&
  zprof

# https://github.com/docker/cli/blob/master/contrib/completion/zsh/_docker
# https://github.com/docker/compose/blob/master/contrib/completion/zsh/_docker-compose
fpath=($ZDOTDIR/completion $fpath)
autoload -Uz compinit && compinit -i

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
