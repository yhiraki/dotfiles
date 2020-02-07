FF_CMD='gof'

_ff-select-repo-dir(){
  local gitroot=$(ghq root)
  local reporoot=$(ghq list \
        | sed s:$gitroot/::g \
        | $FF_CMD)
  if [ ! -z $reporoot ]
  then echo $gitroot/$reporoot
  fi
}

# repo - cd to repogitory dir
repo() {
  local repodir=$(_ff-select-repo-dir "$*")
  if [ ! -z $repodir ]
  then cd $repodir
  fi
}

# checkout
checkout () {
  local branch=$(git branch -a | $FF_CMD | xargs basename)
  local cmd=(git checkout $branch)
  echo $cmd
  $cmd
}

gitroot(){
  cd $(git rev-parse --show-toplevel)
}

_ff-select-files() {
  IFS='
'
  local -a declare files
  files=($(cat - | $FF_CMD))
  # [[ -n "$files" ]] && ${EDITOR:-vim} "${files[@]}"
  [[ -n "$files" ]] && echo "${files[@]}"
  unset IFS
}

fsh() {
  ssh $(cat ~/.ssh/config \
    | grep -i -e '^host' \
    | sed -e 's/host //i' \
    | $FF_CMD)
}

fsql(){
  psql $(cat ~/.pgpass \
     | sed -E 's/:[^:]+$//' \
     | $FF_CMD \
     | sed -e 's/^/-h /' \
       -e 's/:/ -p /' \
       -e 's/:/ -d /' \
       -e 's/:/ -U /')
}

_select-files-in-dir(){
  find $1 -type f | _ff-select-files
}

_select-dirs-in-dir(){
  find $1 -type d | _ff-select-files
}

_select-dirs-in-repo(){
  _select-dirs-in-dir $(_ff-select-repo-dir)
}

_select-files-in-repo(){
  _select-files-in-dir $(_ff-select-repo-dir)
}


alias -g dlf='$(_select-files-in-dir ~/Downloads)'
alias -g dld='$(_select-dirs-in-dir ~/Downloads)'
alias -g junkf='$(_select-files-in-dir ~/.cache/junkfile/)'
alias -g repod='$(_select-dirs-in-repo)'
alias -g repof='$(_select-files-in-repo)'


# 失敗した History は記録しない
# エスケープを含むhistoryが改変されるので無効化
# http://someneat.hatenablog.jp/entry/2017/07/25/073428
# begin
# __record_command() {
#   typeset -g _LASTCMD=${1%%$'\n'}
#   return 1
# }
# zshaddhistory_functions+=(__record_command)

# __update_history() {
#   local last_status="$?"

#   # hist_ignore_space
#   if [[ ! -n ${_LASTCMD%% *} ]]; then
#     return
#   fi

#   # hist_reduce_blanks
#   local cmd_reduce_blanks=$(echo ${_LASTCMD} | tr -s ' ')

#   # Record the commands that have succeeded
#   if [[ ${last_status} == 0 ]]; then
#     print -sr -- "${cmd_reduce_blanks}"
#   fi
# }
# precmd_functions+=(__update_history)
# end
