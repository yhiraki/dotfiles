#!/bin/bash

DOTFILES_REMOTE=https://github.com/awa-manju/dotfiles
ZDOTDIR=$HOME/src/github.com/awa-manju/dotfiles
source $ZDOTDIR/.zsh.d/env.zsh

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


# gitconfig
touch $HOME/.gitconfig
if ! `grep "\[include\]" $HOME/.gitconfig > /dev/null`; then
  cat << EOF >> $HOME/.gitconfig
[include]
	path = $ZDOTDIR/.gitconfig.local
EOF
fi


# execute installers
source $ZDOTDIR/install.d/link.sh
source $ZDOTDIR/install.d/go.sh
source $ZDOTDIR/install.d/ghq.sh
source $ZDOTDIR/install.d/pyenv.sh

sudo chsh $USER --shell $(which zsh)

nvim -c '
set shortmess=a
set nomore
call dein#update()
q'

exec zsh
