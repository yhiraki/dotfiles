#!/bin/bash

export PYENV_ROOT=$HOME/.pyenv

ln -s $gopath/src/github.com/yyuu/pyenv.git $PYENV_ROOT
ln -s $gopath/src/github.com/yyuu/pyenv-virtualenv.git $PYENV_ROOT/plugins/pyenv-virtualenv
