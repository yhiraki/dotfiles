#!/bin/bash

ORIG_DOTFILES=$(echo '
.zshenv
.tmux.conf
.vim
.gitconfig
.git_template
.vimperatorrc
' | xargs)

for f in $ORIG_DOTFILES; do
  if [ -e $HOME/$f ]; then
    mv $HOME/$f /tmp/${f}.orig
  fi
  ln -s $DOTFILES_GIT_PATH/$f $HOME/$f
done

# tmux plugins
TMUXDIR=$HOME/.tmux
mkdir -p $TMUXDIR/plugins
ln -s $HOME/src/github.com/tmux-plugins/tpm $TMUXDIR/plugins/tpm
