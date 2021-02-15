autoload -Uz add-zsh-hook

configure_tmux() {
  if ! command -v tmux >/dev/null; then
    return
  fi

  function my_refresh_tmux_status() {
    if [[ -n "$TMUX" ]]; then
      tmux refresh-client -S
    fi
  }
  add-zsh-hook periodic my_refresh_tmux_status

}
configure_tmux
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
    paulirish/git-open
    robbyrussell/oh-my-zsh/plugins/git
    zsh-users/zsh-autosuggestions
    zsh-users/zsh-completions
    zsh-users/zsh-syntax-highlighting
    yhiraki/zsh-simple-prompt
    # marlonrichert/zsh-autocomplete
  )
  root=$(ghq root)
  local d
  for p in "${plugins_repo[@]}"; do
    d="$root/github.com/$p"
    if [[ ! -d "$d" ]]; then
      echo "No such file $d."
      continue
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

# zmodload zsh/zprof && zprof

# Make status code '0'
echo .zshrc loaded
