function _set_defer () {
  local name=$1
  local setup=$2
  local cmd="_setup_before_execute $setup $name"
  alias "$name=$cmd"
}

function _setup_before_execute () {
  local setup=$1
  local name=$2
  local cmd=$(echo ${@:2:($#-2)})
  unalias $name
  eval $setup
  eval $cmd
}
