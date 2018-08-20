source $ZDOTDIR/global.zsh

# anyenv
export PATH=$ANYENV_ROOT/bin:$PATH
ANYENV_CMD=$ANYENV_ROOT/bin/anyenv
LAZY_CMDS="$($ANYENV_CMD envs) python node npm npx pip go"

function _anyenv_init () {
  eval "$($ANYENV_CMD init -)"
}

function _anyenv_init_and_unalias () {
  for i in $(echo $LAZY_CMDS)
  do
    unalias $i
  done
  _anyenv_init
}

function _anyenv_defer_setup() {
  local i
  local cmd
  for i in $(echo $LAZY_CMDS)
  do
    alias "$i"="_anyenv_init_and_unalias; $i"
  done
}
_anyenv_defer_setup


# gcloud

source $HOME/bin/google-cloud-sdk/path.zsh.inc
