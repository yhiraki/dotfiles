search-history-incremental () {
  history -n 1 | awk '!a[$0]++' | $FF_CMD
}
zle -N search-history-incremental

select-repo(){
  echo $(ghq root)/$(ghq list | $FF_CMD)
}

repo() {
  cd $(select-repo)
}

branch-name () {
  echo $(git branch -a | $FF_CMD)
}

gitroot(){
  cd $(git rev-parse --show-toplevel)
}

fsh() {
  ssh $(cat ~/.ssh/config \
    | grep -i -e '^host' \
    | sed -e 's/host //i' \
    | sed -e '/*/d' \
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
