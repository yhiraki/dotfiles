#!/bin/bash

ln -s $ZDOTDIR/.zshenv $HOME
ln -s $DOTDIR/.tmux.conf $HOME
ln -s $DOTDIR/.vim $HOME
ln -s $DOTDIR/.git_template $HOME
ln -s $DOTDIR/.vimperatorrc $HOME

# tmux plugins
TMUXDIR=$HOME/.tmux
mkdir -p $TMUXDIR/plugins
ln -s $HOME/src/github.com/tmux-plugins/tpm $TMUXDIR/plugins/tpm

# neovim
mkdir -p $XDG_CONFIG_HOME/nvim
ln -s $DOTDIR/.vim/vimrc $XDG_CONFIG_HOME/nvim/init.vim
