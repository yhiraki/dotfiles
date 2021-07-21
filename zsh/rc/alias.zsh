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
alias -g C='| pbcopy'
alias -g WP="| sed -e 's:^.*@://:' | tr '/' '\\\\'"
