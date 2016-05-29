#!/bin/bash

ORIG_DOTFILES=(
.zshrc
.zsh.d
.tmux.conf
.vim
.gitconfig
)

for f in $ORIG_DOTFILES; do
  if [ -e $HOME/$f ]; then
    mv $HOME/$f /tmp${f}.orig
  fi
  ln -s $DOTFILES_GIT_PATH/$f $HOME/$f
done
