alias ...='cd ../..'
alias ....='cd ../../..'

alias ls="ls --color=auto -F"

alias -g dlf='$(find-dir ~/Downloads)'
alias -g junkf='$(find-dir ~/.cache/junkfile/)'
alias -g repof='$(find-dir $(select-repo))'

alias -g bra='$(basename $(branch-name))'
