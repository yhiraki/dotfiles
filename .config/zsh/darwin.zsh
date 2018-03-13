alias ls="ls -G"
alias ll="ls -lG"
alias la="ls -laG"
alias sed="gsed"
alias updatedb='/usr/libexec/locate.updatedb'

function nas () {
  local r_path="$(
    echo -E $1 \
      | awk '{$1=$1; print}' \
      | tr '\' '/' \
      | tr -d '\n' \
      | sed -e 's/^  *//g' \
            -e 's/  *$//g' \
            -e 's://*:/:g')"
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

  if [ -d "$l_path" ]; then
    cd "$l_path"
  else
    open "$l_path"
    cd "$(dirname $l_path)"
  fi
}

function winpath () {
  local _dir _file

  if [ $# -eq 1 ]; then
    _file=$1
  fi

  _dir=$(pwd)

  if echo $_dir | grep '^/Volumes/' > /dev/null; then
    _dir=$(echo $_dir | sed s/Volumes// | sed s:_:/:)
  fi

  echo $_dir/$_file | tr '/' '\\'
}

# vim:ft=zsh
