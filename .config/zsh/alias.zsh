alias ma='my-manual'

alias ...='cd ../..'
alias ....='cd ../../..'

function emacsclient-gui {
  local filename=$1
  emacsclient -c $filename &
}
alias e=$EDITOR
alias ge=emacsclient-gui

alias -g F="| fzf "
alias -g FP="| fzf --preview '$FZF_PREVIEW_CMD'"
alias -g O='F | xargs open'
