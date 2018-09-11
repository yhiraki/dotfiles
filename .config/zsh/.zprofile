source $ZDOTDIR/global.zsh

# anyenv
export PATH=$ANYENV_ROOT/bin:$PATH
ANYENV_CMD=$ANYENV_ROOT/bin/anyenv
eval "$($ANYENV_CMD init -)"

# gcloud
source $HOME/bin/google-cloud-sdk/path.zsh.inc
