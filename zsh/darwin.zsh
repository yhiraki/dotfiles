alias ls="ls -G"
alias ll="ls -lG"
alias la="ls -laG"
alias sed="gsed"
alias updatedb='/usr/libexec/locate.updatedb'

function nas () {
  local r_path="$(echo -E /$1 \
      | tr '\' '/' \
      | tr -d '\n' \
      | sed 's/^  *//g' \
      | sed 's/  *$//g' \
      | sed 's://*:/:g')"
  local r_dir="/$(echo $r_path | cut -d '/' -f-3)"
  local l_root="/Volumes"
  local l_dir="$l_root/$(echo $r_path | cut -d '/' -f2-3 | tr '/' '_')"
  local l_path="$l_dir/$(echo $r_path | cut -d '/' -f4-)"

  if [ ! -d $l_dir ]; then
    mkdir $l_dir
  fi
  if ! mount | grep $l_dir > /dev/null; then
    mount -t smbfs $r_dir $l_dir
  fi

  # open "$l_path"
  if [ -d "$l_path" ]; then
    cd "$l_path"
  else
    cd "$(dirname $l_path)"
  fi
}

function winpath(){
  local _dir

  if [ $# -eq 1 ]; then
    _dir=$1
  else
    _dir=$(pwd)
  fi

  if echo $_dir | grep '^/Volumes/' > /dev/null; then
    _dir=$(echo $_dir | sed s/Volumes// | sed s:_:/:)
  fi

  echo $_dir | tr '/' '\\'
}

# vim:ft=zsh
