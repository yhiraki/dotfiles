ff () {
  $(echo ${FF_CMD} ${FF_OPTIONS})
}

search-history-incremental () {
  BUFFER=$(history -n 1 | awk '!a[$0]++' | ff)
  CURSOR=$#BUFFER
  zle reset-prompt
}
zle -N search-history-incremental

select-repo(){
  echo $(ghq root)/$(ghq list | ff)
}

repo() {
  cd $(select-repo)
}

branch-name () {
  echo $(git branch -a | ff)
}

gitroot(){
  cd $(git rev-parse --show-toplevel)
}

fsh() {
  ssh $(cat ~/.ssh/config \
    | grep -i -e '^host' \
    | sed -e 's/host //i' \
          -e '/*/d' \
    | ff)
}

fsql(){
  psql $(
    cat ~/.pgpass \
      | sed -E 's/:[^:]+$//' \
      | ff \
      | sed -e 's/^/-h /' \
            -e 's/:/ -p /' \
            -e 's/:/ -d /' \
            -e 's/:/ -U /')
}

find-dir () {
  local d=$(echo $1 | sed -e "s:~:$HOME:")
  echo "$d/"$(
    find "$d" 2> /dev/null \
      | sed -e "s:^$d/\?::" \
            -e '/^$/d' \
      | ${FF_CMD})
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
