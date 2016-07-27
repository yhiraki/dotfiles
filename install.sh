#!/bin/bash

DOTFILES_REMOTE=https://github.com/awa-manju/dotfiles
DOTDIR=$HOME/src/github.com/awa-manju/dotfiles

# install packages
if which apt-get > /dev/null; then
  sudo apt-get -y install git
  git clone $DOTFILES_REMOTE $DOTDIR
  source $DOTDIR/zsh/env.zsh
  source $DOTDIR/install.d/apt.sh

elif which brew > /dev/null; then
  brew install git
  git clone $DOTFILES_REMOTE $DOTDIR
  source $DOTDIR/zsh/env.zsh
  source $DOTDIR/install.d/brew.sh

# elif which pacman > /dev/null; then
#   source $HOME/install.d/pacman.sh
fi


# gitconfig
touch $HOME/.gitconfig
if ! `grep "\[include\]" $HOME/.gitconfig > /dev/null`; then
  cat << EOF >> $HOME/.gitconfig
[include]
	path = $DOTDIR/.gitconfig.local
EOF
fi


# execute installers
source $DOTDIR/install.d/link.sh
source $DOTDIR/install.d/go.sh
source $DOTDIR/install.d/ghq.sh
source $DOTDIR/install.d/pyenv.sh
source $DOTDIR/install.d/plantuml.sh

sudo chsh $USER --shell $(which zsh)

nvim -c "
set shortmess=a
set nomore
call dein#update()
q"

exec zsh
