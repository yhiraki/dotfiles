#!/bin/bash

ORIG_DOTFILES=$(echo '
.zshrc
.zsh.d
.tmux.conf
.vim
.gitconfig
' | xargs)

for f in $ORIG_DOTFILES; do
  if [ -e $HOME/$f ]; then
    mv $HOME/$f /tmp${f}.orig
  fi
  ln -s $DOTFILES_GIT_PATH/$f $HOME/$f
done
