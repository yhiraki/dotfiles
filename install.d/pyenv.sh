#!/bin/bash

export PYENV_ROOT=$HOME/.pyenv

ln -s $GOPATH/src/github.com/yyuu/pyenv $PYENV_ROOT
ln -s $GOPATH/src/github.com/yyuu/pyenv-virtualenv $PYENV_ROOT/plugins/pyenv-virtualenv
