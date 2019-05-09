#!/bin/bash

title () {
  echo
  echo --------------------------------------------------------------------------------
  echo $*
  echo --------------------------------------------------------------------------------
}

DOTFILES_REPO=yhiraki/dotfiles

# ----------------------------------------------------------------------
title "Install brew"
# ----------------------------------------------------------------------

if ! which brew
then
  /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi

# ----------------------------------------------------------------------
title "Fetch repo"
# ----------------------------------------------------------------------

brew install ghq
export GOPATH=$HOME

ghq get $DOTFILES_REPO
DOTDIR=$GOPATH/src/github.com/$DOTFILES_REPO
test -d $DOTDIR || exit 1

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

# ----------------------------------------------------------------------
title "Install essentials"
# ----------------------------------------------------------------------

brew install wget fzf

# ----------------------------------------------------------------------
title "gcc g++"
# ----------------------------------------------------------------------

brew install gcc 
ln -s /usr/local/bin/gcc-8 ~/bin/gcc

brew install llvm
ln -s /usr/local/bin/g++-8 ~/bin/g++

# ----------------------------------------------------------------------
title "Install plantuml"
# ----------------------------------------------------------------------

brew install graphviz

LIB_JAVA_DIR=$HOME/lib/java
PLANTUML_JAR=$LIB_JAVA_DIR/plantuml.jar

if [ ! -f $PLANTUML_JAR ]; then
  mkdir -p $LIB_JAVA_DIR
  wget http://downloads.sourceforge.net/project/plantuml/plantuml.jar -O $PLANTUML_JAR
fi

ln -s $DOTDIR/plantuml/ ~/.confg/plantuml

# ----------------------------------------------------------------------
title "Install neovim"
# ----------------------------------------------------------------------

brew install neovim/neovim/neovim
ln -s $DOTDIR/vim/ ~/.vim
ln -s ~/.vim ~/.config/nvim

# ----------------------------------------------------------------------
title "Mac defaults"
# ----------------------------------------------------------------------

# Finderで隠しファイルを表示する
defaults write com.apple.finder AppleShowAllFiles TRUE

# マウスカーソル速度
defaults write -g com.apple.trackpad.scaling 5

# ----------------------------------------------------------------------
title "Utils"
# ----------------------------------------------------------------------

brew install coreutils findutils gnu-sed gnu-tar grep

# ----------------------------------------------------------------------
title "Install tmux"
# ----------------------------------------------------------------------

brew install tmux
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
ln -s $DOTDIR/.tmux.conf ~/

# ----------------------------------------------------------------------
title "Install zsh"
# ----------------------------------------------------------------------

brew install zsh
ln -s $DOTDIR/zsh ~/.config/zsh
ln -s ~/.config/zsh/.zshenv ~/

# ----------------------------------------------------------------------
title "Install languages"
# ----------------------------------------------------------------------

brew install python3 go node

# ----------------------------------------------------------------------
title "Install alacritty"
# ----------------------------------------------------------------------

brew cask install alacritty
ln -s $DOTDIR/alacritty ~/.config/alacritty

# ----------------------------------------------------------------------
title "done."
