_fzf-select-repo-dir(){
  local gitroot=$(ghq root)
  local reporoot=$(ghq list \
        | sed s:$gitroot/::g \
        | $FZF_CMD -q "$*")
  if [ ! -z $reporoot ]
  then echo $gitroot/$reporoot
  fi
}

# repo - cd to repogitory dir
repo() {
  local repodir=$(_fzf-select-repo-dir "$*")
  echo $repodir
  if [ ! -z $repodir ]
  then cd $repodir
  fi
}

# fshow - git commit browser
gitshow() {
  git log --graph --color=always \
      --format="%C(auto)%h%d %s %C(black)%C(bold)%cr" "$@" |
  fzf --ansi --no-sort --reverse --tiebreak=index --bind=ctrl-s:toggle-sort \
      --bind "ctrl-m:execute:
                (grep -o '[a-f0-9]\{7\}' | head -1 |
                xargs -I % sh -c 'git show --color=always % | less -R') << 'FZF-EOF'
                {}
FZF-EOF"
}

gitroot(){
  cd $(git rev-parse --show-toplevel)
}


_fzf-select-files() {
  IFS='
'
  local -a declare files
  files=($(cat - | $FZF_CMD --query="$1" -m --select-1 --exit-0))
  # [[ -n "$files" ]] && ${EDITOR:-vim} "${files[@]}"
  [[ -n "$files" ]] && echo "${files[@]}"
  unset IFS
}

fsh() {
  ssh $(cat ~/.ssh/config \
    | grep -i -e '^host' \
    | sed -e 's/host //i' \
    | $FZF_CMD -q "$*")
}

fsql(){
  psql $(cat ~/.pgpass \
     | sed -E 's/:[^:]+$//' \
     | $FZF_CMD -q "$*" \
     | sed -e 's/^/-h /' \
       -e 's/:/ -p /' \
       -e 's/:/ -d /' \
       -e 's/:/ -U /')
}

_select-files-in-dir(){
  find $1 -type f | _fzf-select-files
}

_select-dirs-in-dir(){
  find $1 -type d | _fzf-select-files
}

_select-dirs-in-repo(){
  _select-dirs-in-dir $(_fzf-select-repo-dir)
}

_select-files-in-repo(){
  _select-files-in-dir $(_fzf-select-repo-dir)
}


alias -g dlf='$(_select-files-in-dir ~/Downloads)'
alias -g dld='$(_select-dirs-in-dir ~/Downloads)'
alias -g junkf='$(_select-files-in-dir ~/.cache/junkfile/)'
alias -g repod='$(_select-dirs-in-repo)'
alias -g repof='$(_select-files-in-repo)'


# 失敗した History は記録しない
# http://someneat.hatenablog.jp/entry/2017/07/25/073428
# begin
__record_command() {
  typeset -g _LASTCMD=${1%%$'\n'}
  return 1
}
zshaddhistory_functions+=(__record_command)

__update_history() {
  local last_status="$?"

  # hist_ignore_space
  if [[ ! -n ${_LASTCMD%% *} ]]; then
    return
  fi

  # hist_reduce_blanks
  local cmd_reduce_blanks=$(echo ${_LASTCMD} | tr -s ' ')

  # Record the commands that have succeeded
  if [[ ${last_status} == 0 ]]; then
    print -sr -- "${cmd_reduce_blanks}"
  fi
}
precmd_functions+=(__update_history)
# end
