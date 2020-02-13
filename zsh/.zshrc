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

GHQROOT=$(ghq root)
source $GHQROOT/github.com/'paulirish/git-open'/git-open.plugin.zsh	
source $GHQROOT/github.com/'robbyrussell/oh-my-zsh/plugins/git'/git.plugin.zsh
source $GHQROOT/github.com/'zsh-users/zsh-autosuggestions'/zsh-autosuggestions.plugin.zsh
source $GHQROOT/github.com/'zsh-users/zsh-completions'/zsh-completions.plugin.zsh	
source $GHQROOT/github.com/'zsh-users/zsh-syntax-highlighting'/zsh-syntax-highlighting.plugin.zsh

source $ZDOTDIR/rc/alias.zsh
source $ZDOTDIR/rc/func.zsh
source $ZDOTDIR/rc/bind.zsh


PS1='\$ '

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
