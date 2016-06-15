#!/bin/bash

export PYENV_ROOT=$HOME/.pyenv

ln -s $GOPATH/src/github.com/yyuu/pyenv $PYENV_ROOT

mkdir $PYENV_ROOT/plugins
ln -s $GOPATH/src/github.com/yyuu/pyenv-virtualenv $PYENV_ROOT/plugins/pyenv-virtualenv
ln -s $GOPATH/src/github.com/jawshooah/pyenv-default-packages $PYENV_ROOT/plugins/pyenv-default-packages
ln -s $DOTDIR/pyenv/default-packages $PYENV_ROOT/

source $ZDOTDIR/.zprofile

pyenv install 3.5.1
pyenv global 3.5.1
