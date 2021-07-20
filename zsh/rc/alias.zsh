function emacsclient-gui() {
  bash -c "emacsclient -c -a '' $1 &"
}

alias e='emacsclient -nw -a ""'
alias ge=emacsclient-gui

alias ...='cd ../..'
alias ....='cd ../../..'

alias ls="ls -FG"

alias -g F='| ff'
alias -g O='| xargs open'
