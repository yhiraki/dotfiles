configure_tmux() {
  if ! command -v tmux >/dev/null; then
    return
  fi

  if [[ -z "$TMUX" ]]; then
    if tmux list-session >/dev/null; then
      exec tmux a
    else
      exec tmux new-session
    fi
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
