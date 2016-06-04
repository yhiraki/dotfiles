#!/bin/bash

DOTFILES_REMOTE=https://github.com/awa-manju/dotfiles
ZDOTDIR=$HOME/src/github.com/awa-manju/dotfiles

# install packages
if which apt-get > /dev/null; then
  sudo apt-get -y install git
  git clone $DOTFILES_REMOTE $ZDOTDIR
  source $ZDOTDIR/install.d/apt.sh

elif which brew > /dev/null; then
  brew install git
  git clone $DOTFILES_REMOTE $ZDOTDIR
  source $ZDOTDIR/install.d/brew.sh

# elif which pacman > /dev/null; then
#   source $HOME/install.d/pacman.sh
fi

source $ZDOTDIR/install.d/{link,go,ghq,pyenv}.sh

# gitconfig
touch $HOME/.gitconfig
if ! `grep "\[include\]" $HOME/.gitconfig > /dev/null`; then
  cat << EOF >> $HOME/.gitconfig
[include]
	path = $ZDOTDIR/.gitconfig.local
EOF
fi

sudo chsh $USER --shell $(which zsh)
exit
