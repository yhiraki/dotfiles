alias ma='my-manual'

alias ...='cd ../..'
alias ....='cd ../../..'

function emacsclient-gui {
  local filename=$1
  $EMACSCLIENT -c $filename & disown
}
alias e="$EDITOR"
alias ge="emacsclient-gui"

function locate-and-open {
  local query=$1
  locate -i $query | fzf | xargs open
}
alias f=locate-and-open

alias -g F="| fzf "
alias -g FP="| fzf --preview '$FZF_PREVIEW_CMD'"
alias -g O='F | xargs open'

alias gcloud='_gcloud gcloud'
alias gsutil='_gcloud gsutil'
alias bq='_gcloud bq'

alias ls="ls --color=auto -F"
