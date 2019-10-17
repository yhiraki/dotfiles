alias ...='cd ../..'
alias ....='cd ../../..'

function locate-and-open {
  local query=$1
  locate -i $query | fzf | xargs open
}
alias f=locate-and-open

alias -g F="| fzf "
alias -g FP="| fzf --preview '$FZF_PREVIEW_CMD'"
alias -g O='F | xargs open'

alias ls="ls --color=auto -F"
