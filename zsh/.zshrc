autoload -Uz add-zsh-hook

configure_tmux() {
  if ! command -v tmux >/dev/null; then
    return
  fi

  if [[ -n "$TMUX" ]]; then
	function my_refresh_tmux_status() {
	  tmux refresh-client -S
	}
	add-zsh-hook periodic my_refresh_tmux_status
  else
	if tmux list-sessions; then
	  exec tmux a
	else
	  exec tmux
	fi
  fi
}
[[ -z "$INSIDE_EMACS" ]] && configure_tmux
unset -f configure_tmux

load_plugins() {
  plugins_local=(
    func.zsh
    alias.zsh
    bind.zsh
  )
  for p in "${plugins_local[@]}"; do
    source "$ZDOTDIR/rc/$p"
  done

  command -v ghq >/dev/null || return

  plugins_repo=(
    github.com/paulirish/git-open
    github.com/robbyrussell/oh-my-zsh/plugins/git
    github.com/zsh-users/zsh-autosuggestions
    github.com/zsh-users/zsh-completions
    github.com/zsh-users/zsh-syntax-highlighting
    github.com/yhiraki/zsh-simple-prompt
    # marlonrichert/zsh-autocomplete
  )
  root=$(ghq root)
  local d
  for p in "${plugins_repo[@]}"; do
    d="$root/$p"
    if [[ ! -d "$d" ]]; then
      if command -v ghq > /dev/null; then
	ghq get "$p"
	else
      continue
      fi
    fi
    source $d/*.plugin.zsh
  done
}
load_plugins
unset -f load_plugins

# direnv setup
command -v direnv >/dev/null &&
  eval "$(direnv hook zsh)"

# path sort by string length
export PATH=$(echo "$PATH" |
  tr : '\n' |
  awk '{print length(), $0}' |
  sort -nr |
  cut -d ' ' -f 2 |
  tr '\n' :)

[[ -f "$HOME/.zshrc.local" ]] && source "$HOME/.zshrc.local"

command -v zprof >/dev/null &&
  zprof

# https://github.com/docker/cli/blob/master/contrib/completion/zsh/_docker
# https://github.com/docker/compose/blob/master/contrib/completion/zsh/_docker-compose
fpath=($ZDOTDIR/completion $fpath)
autoload -Uz compinit && compinit -i

# emacs vterm
if [[ "$INSIDE_EMACS" = 'vterm' ]] \
    && [[ -n ${EMACS_VTERM_PATH} ]] \
    && [[ -f ${EMACS_VTERM_PATH}/etc/emacs-vterm-zsh.sh ]]; then
    source ${EMACS_VTERM_PATH}/etc/emacs-vterm-zsh.sh
    # Initialize TITLE
    print -Pn "\e]2;%m:%2~\a"
fi

# zmodload zsh/zprof && zprof

# Make status code '0'
echo .zshrc loaded
