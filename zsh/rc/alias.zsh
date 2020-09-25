case ${OSTYPE} in
  darwin*)
    EMACSCLIENT="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient"
    ;;
  linux*) 
    EMACSCLIENT=$(command -v emacsclient)
    ;;
esac

function emacsclient-gui() {
  local filename=$1
  bash -c "$EMACSCLIENT -c -a '' $filename &"
}

alias e='$EMACSCLIENT -nw -a ""'
alias ge=emacsclient-gui

alias ...='cd ../..'
alias ....='cd ../../..'

alias ls="ls --color=auto -F"
