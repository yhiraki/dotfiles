ANYENV_CMD=$ANYENV_ROOT/bin/anyenv

function anyenv_init () {
  local cmd
  cmd=$1
  args=$(echo ${@:2:($#-2)})
  unalias_anyenv_init
  eval "$($ANYENV_CMD init -)"
  $cmd $args
}

function alias_anyenv_init () {
  for i in $($ANYENV_CMD envs)
  do
    alias "$i=anyenv_init $i"
  done
}
alias_anyenv_init

function unalias_anyenv_init () {
  for i in $($ANYENV_CMD envs)
  do
    unalias $i
  done
}

source $HOME/bin/google-cloud-sdk/path.zsh.inc
