#!/bin/bash

DOTFILES_GIT_URL=https://github.com/awa-manju/dotfiles
DOTFILES_GIT_PATH=$HOME/src/github.com/awa-manju/dotfiles

# install packages
if which apt-get > /dev/null; then
  sudo apt-get -y install git
  git clone $DOTFILES_GIT_URL $DOTFILES_GIT_PATH
  source $DOTFILES_GIT_PATH/install.d/apt.sh

elif which brew > /dev/null; then
  brew install git
  git clone $DOTFILES_GIT_URL $DOTFILES_GIT_PATH
  source $DOTFILES_GIT_PATH/install.d/brew.sh

# elif which pacman > /dev/null; then
#   source $HOME/install.d/pacman.sh
fi

source $DOTFILES_GIT_PATH/install.d/link.sh
source $DOTFILES_GIT_PATH/install.d/go.sh
source $DOTFILES_GIT_PATH/install.d/ghq.sh
source $DOTFILES_GIT_PATH/install.d/pyenv.sh

sudo chsh $USER --shell $(which zsh)
exit
