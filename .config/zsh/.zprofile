source $ZDOTDIR/global.zsh

# anyenv
export PATH=$ANYENV_ROOT/bin:$PATH
ANYENV_CMD=$ANYENV_ROOT/bin/anyenv

function _anyenv_init () {
  eval "$($ANYENV_CMD init -)"
}

function _anyenv_defer_setup() {
  local i
  local cmd
  for i in $($ANYENV_CMD envs)
  do
    _set_defer $i _anyenv_init
  done
}
_anyenv_defer_setup


# gcloud

source $HOME/bin/google-cloud-sdk/path.zsh.inc
