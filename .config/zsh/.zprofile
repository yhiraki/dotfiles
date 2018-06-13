ANYENV_INIT_CMD=$ANYENV_ROOT/anyenv-init.sh
if [ ! -f $ANYENV_INIT_CMD ]
then
  anyenv init - --no-rehash > $ANYENV_INIT_CMD
fi

if [ -d $ANYENV_ROOT ]
then
    export PATH="$ANYENV_ROOT/bin:$PATH"
    source $ANYENV_INIT_CMD
fi

source $HOME/bin/google-cloud-sdk/path.zsh.inc
