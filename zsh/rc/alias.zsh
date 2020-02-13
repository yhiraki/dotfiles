alias ...='cd ../..'
alias ....='cd ../../..'

alias ls="ls --color=auto -F"

alias -g dlf='$(find ~/Downloads -type f | '"$FF_CMD"')'
alias -g dld='$(find ~/Downloads -type d | '"$FF_CMD"')'
alias -g junkf='$(find ~/.cache/junkfile/ -type f | '"$FF_CMD"')'
alias -g junkd='$(find ~/.cache/junkfile/ -type d | '"$FF_CMD"')'
alias -g repof='$(find $(select-repo) -type f | '"$FF_CMD"')'
alias -g repod='$(find $(select-repo) -type d | '"$FF_CMD"')'
alias -g bra='$(branch-name)'
