autoload -Uz add-zsh-hook

if [ -z "$TMUX" ]
then
  if tmux list-session > /dev/null
  then
    exec tmux a
  else
    exec tmux new-session
  fi
fi

function my_refresh_tmux_status() {
  if [ ! -z $TMUX ]; then
    tmux refresh-client -S
  fi
}
add-zsh-hook periodic my_refresh_tmux_status

case ${OSTYPE} in
darwin*)
  EMACSCLIENT="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient"

  function emacsclient-gui {
    local filename=$1
    bash -c "$EMACSCLIENT -c -a '' $filename &"
  }

  alias e="$EMACSCLIENT -nw -a ''"
  alias ge=emacsclient-gui
  ;;
linux*)
  ;;
msys*)
  if which start > /dev/null; then
    function mstart(){
      for arg in $@
      do
        start $arg
      done
    }
    alias start=mstart
  fi
  ;;
esac

local GHQROOT=$(ghq root)
local plugins_repo=(
  paulirish/git-open
  robbyrussell/oh-my-zsh/plugins/git
  zsh-users/zsh-autosuggestions
  zsh-users/zsh-completions
  zsh-users/zsh-syntax-highlighting
  yhiraki/zsh-simple-prompt
)
for p in $plugins_repo
do
  source $GHQROOT/github.com/$p/*.plugin.zsh
done

local plugins_local=(
  func.zsh
  alias.zsh
  bind.zsh
)
for p in $plugins_local
do
  source $ZDOTDIR/rc/$p
done

# direnv setup
which direnv > /dev/null \
  && eval "$(direnv hook zsh)"

# zsh-autosuggestions
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=242'
bindkey '^ ' autosuggest-accept

# path sort by string length
export PATH=$(echo $PATH \
         | tr : '\n' \
         | awk '{print length(), $0}' \
         | sort -nr \
         | cut -d ' ' -f 2 \
         | tr '\n' :)

[ -f ~/.zshrc.local ] && source ~/.zshrc.local

which zprof > /dev/null \
  && zprof

# zmodload zsh/zprof && zprof

# Make status code '0'
echo .zshrc loaded
