#!/bin/bash

ORIG_DOTFILES=(
.zshrc
.zsh.d
.tmux.conf
.vim
)

for f in $ORIG_DOTFILES; do
  if [ -e $DOTFILES_GIT_PATH/$f ]; then
    mv $DOTFILES_GIT_PATH/$f{,.orig}
  fi
  ln -s $DOTFILES_GIT_PATH/$f $HOME/$f
done
