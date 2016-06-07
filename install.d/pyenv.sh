#!/bin/bash

export PYENV_ROOT=$HOME/.pyenv

mkdir $PYENV_ROOT/plugins

ln -s $GOPATH/src/github.com/yyuu/pyenv $PYENV_ROOT
ln -s $GOPATH/src/github.com/yyuu/pyenv-virtualenv $PYENV_ROOT/plugins/pyenv-virtualenv
ln -s $GOPATH/src/github.com/jawshooah/pyenv-default-packages $PYENV_ROOT/plugins/pyenv-default-packages
ln -s $ZDOTPATH/pyenv/default-packages $PYENV_ROOT/
