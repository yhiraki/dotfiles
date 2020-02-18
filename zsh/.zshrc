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

source $ZDOTDIR/rc/func.zsh
source $ZDOTDIR/rc/alias.zsh
source $ZDOTDIR/rc/bind.zsh

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

autoload -Uz colors && colors

signal_name () {
  local sigs=$(cat << EOF
01 SIGHUP
02 SIGINT
03 SIGQUIT
04 SIGILL
05 SIGTRAP
06 SIGABRT
07 SIGEMT
08 SIGFPE
09 SIGKILL
10 SIGBUS
11 SIGSEGV
12 SIGSYS
13 SIGPIPE
14 SIGALRM
15 SIGTERM
16 SIGURG
17 SIGSTOP
18 SIGTSTP
19 SIGCONT
20 SIGCHLD
21 SIGTTIN
22 SIGTTOU
23 SIGIO
24 SIGXCPU
25 SIGXFSZ
26 SIGVTALRM
27 SIGPROF
28 SIGWINCH
29 SIGINFO
30 SIGUSR1
31 SIGUSR2
EOF
)
  echo $sigs | grep $(printf %02d $1) | awk '{print $2}'
}

check_last_exit_code() {
  local code=$?
  if [ $code -gt 128 ]
  then
    local sigid=$(($code - 128))
    echo "%{$fg_bold[yellow]%}$(signal_name $sigid)%{$reset_color%}($sigid)"
    return
  fi
  if [ $code -ne 0 ]
  then
    echo "%{$fg_bold[red]%}$code%{$reset_color%}"
    return
  fi
}

preexec() {
  cmd=$1
  if [ -n "$cmd" ]
  then
    timer=$(($(date +%s%0N)/1000000))
  fi
}

precmd() {
  unset elapsed
  if [ -n "$timer" ]
  then
    now=$(($(date +%s%0N)/1000000))
    elapsed=$(($now-$timer))
    unset timer
  fi
}

check_elapsed_time() {
  if [ -n "$elapsed" ]
  then
    local msec=1

    if [ $elapsed -lt $(($msec * 500)) ]
    then
      local ms=$(($elapsed / ${msec}))
      echo "%{$fg_bold[green]%}$ms%{$reset_color%}ms"
      return
    fi

    local sec=$(($msec * 1000))
    local min=$(($sec * 60))

    if [ $elapsed -lt $min ]
    then
      local s=$(($elapsed / ${sec}.0))
      echo "%{$fg_bold[green]%}$(printf %.1f $s)%{$reset_color%}s"
      return
    fi

    local hour=$(($min * 60))

    if [ $elapsed -lt $hour ]
    then
      local m=$(($elapsed / $min))
      local s=$(($(($elapsed % $min)) / $sec))
      echo -n "%{$fg_bold[green]%}$m%{$reset_color%}m"
      echo "%{$fg_bold[green]%}$s%{$reset_color%}s"
      return
    fi

    local h=$(($elapsed / $hour))
    local m=$(($(($elapsed % $hour)) / $min))
    echo -n "%{$fg_bold[green]%}$h%{$reset_color%}h"
    echo "%{$fg_bold[green]%}$m%{$reset_color%}m"
  fi
}

prompt() {
  local code=$?
  if [ $code -eq 0 ]
  then
    echo "%{$fg_bold[green]%}$%{$reset_color%} "
    return
  fi
  echo "%{$fg_bold[red]%}$%{$reset_color%} "
}

rprompt() {
  echo $(check_last_exit_code) $(check_elapsed_time)
}

PROMPT='$(prompt)'
RPROMPT='$(rprompt)'

[ -f ~/.zshrc.local ] && source ~/.zshrc.local

which zprof > /dev/null \
  && zprof

# zmodload zsh/zprof && zprof

# Make status code '0'
echo .zshrc loaded
