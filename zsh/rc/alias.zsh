alias ...='cd ../..'
alias ....='cd ../../..'

alias ls="ls --color=auto -F"

alias -g dlf='$(find-file ~/Downloads)'
alias -g junkf='$(find-file ~/.cache/junkfile/)'
alias -g repof='$(find-file $(select-repo))'

alias -g bra='$(basename $(branch-name))'
