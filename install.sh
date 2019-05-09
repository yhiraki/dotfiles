#!/bin/bash

GIT_REPO=yhiraki/dotfiles

if which brew > /dev/null; then
  /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi

brew install ghq

export GOPATH=$HOME
ghq get yhiraki/dotfiles

# source $DOTDIR/zsh/env.zsh
# source $DOTDIR/install.d/brew.sh

# # gitconfig
# touch $HOME/.gitconfig
# if ! `grep "\[include\]" $HOME/.gitconfig > /dev/null`; then
#   cat << EOF >> $HOME/.gitconfig
# [include]
# 	path = $DOTDIR/.gitconfig.local
# EOF
# fi


# # execute installers
# bash $DOTDIR/install.d/link.sh
# bash $DOTDIR/install.d/go.sh
# bash $DOTDIR/install.d/ghq.sh
# bash $DOTDIR/install.d/plantuml.sh
