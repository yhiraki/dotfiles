autoload -Uz add-zsh-hook

if [[ -z "$TMUX" ]]; then
  if tmux list-session >/dev/null; then
    exec tmux a
  else
    exec tmux new-session
  fi
fi

function my_refresh_tmux_status() {
  if [[ -n $TMUX ]]; then
    tmux refresh-client -S
  fi
}
add-zsh-hook periodic my_refresh_tmux_status

case ${OSTYPE} in
  darwin*)
    EMACSCLIENT="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient"

    function emacsclient-gui() {
      local filename=$1
      bash -c "$EMACSCLIENT -c -a '' $filename &"
    }

    alias e='$EMACSCLIENT -nw -a ""'
    alias ge=emacsclient-gui
    ;;
  linux*) ;;

  msys*)
    if command -v start >/dev/null; then
      function mstart() {
        for arg in "$@"; do
          start "$arg"
        done
      }
      alias start=mstart
    fi
    ;;
esac

plugins_repo=(
  paulirish/git-open
  robbyrussell/oh-my-zsh/plugins/git
  zsh-users/zsh-autosuggestions
  zsh-users/zsh-completions
  zsh-users/zsh-syntax-highlighting
  yhiraki/zsh-simple-prompt
)
GHQROOT=$(ghq root)
for p in "${plugins_repo[@]}"; do
  source $GHQROOT/github.com/$p/*.plugin.zsh
done

plugins_local=(
  func.zsh
  alias.zsh
  bind.zsh
)
for p in "${plugins_local[@]}"; do
  source "$ZDOTDIR/rc/$p"
done

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

# zmodload zsh/zprof && zprof

# Make status code '0'
echo .zshrc loaded
