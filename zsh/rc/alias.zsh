function emacsclient-gui() {
  bash -c "emacsclient -c -a '' $1 &"
}

alias e='emacsclient -nw -a ""'
alias ge=emacsclient-gui

alias ...='cd ../..'
alias ....='cd ../../..'

alias ls="ls --color=auto -F"

alias -g F='| ff'
alias -g O='| xargs open'
