#!/bin/bash

BASEURL=https://yhiraki.github.io/dotfiles
DOTFILES_REPO=yhiraki/dotfiles
XDG_CONFIG_HOME="$HOME/.config"

title () {
  echo
  echo --------------------------------------------------------------------------------
  echo $*
  echo --------------------------------------------------------------------------------
}

# ----------------------------------------------------------------------
title "Prepare"
# ----------------------------------------------------------------------

mkdir $XDG_CONFIG_HOME

# ----------------------------------------------------------------------
title "Xcode command line tools"
# ----------------------------------------------------------------------

xcode-select --install

# ----------------------------------------------------------------------
title "Install brew"
# ----------------------------------------------------------------------

if ! which brew
then
  /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi

# ----------------------------------------------------------------------
title "Git settings"
# ----------------------------------------------------------------------

touch $HOME/.gitconfig
[ -e ~/.gitconfig.local ] || curl $BASEURL/.gitconfig.local -o ~/.gitconfig.local
git config --global include.path '~/.gitconfig.local'

# ----------------------------------------------------------------------
title "Install ghq"
# ----------------------------------------------------------------------

brew install ghq
[ $(ghq root) == "${HOME}/src" ] || exit 1

# ----------------------------------------------------------------------
title "Fetch repo"
# ----------------------------------------------------------------------

ghq get $DOTFILES_REPO
DOTDIR=$HOME/src/github.com/$DOTFILES_REPO
[ -d $DOTDIR ] || exit 1

if [ ! -L ~/.gitconfig.local ]
then
  rm ~/.gitconfig.local
  ln -s $DOTDIR/.gitconfig.local ~/
fi

# ----------------------------------------------------------------------
title "Install essentials"
# ----------------------------------------------------------------------

brew install \
  ag \
  direnv \
  fzf \
  gibo \
  gnupg \
  terminal-notifier \
  wget

# ----------------------------------------------------------------------
title "gcc g++"
# ----------------------------------------------------------------------

brew install gcc 
ln -s /usr/local/bin/gcc-8 ~/bin/gcc

brew install llvm
ln -s /usr/local/bin/g++-8 ~/bin/g++

brew install gdb

# brew tap twlz0ne/homebrew-ccls
# brew install ccls

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

ln -s $DOTDIR/plantuml/ $XDG_CONFIG_HOME

# ----------------------------------------------------------------------
title "Install neovim"
# ----------------------------------------------------------------------

brew install neovim/neovim/neovim
[ -e ~/.vim ] || ln -s $DOTDIR/vim/ ~/.vim
[ -e $XDG_CONFIG_HOME/nvim ] || ln -s ~/.vim $XDG_CONFIG_HOME/nvim

# ----------------------------------------------------------------------
title "Mac defaults"
# ----------------------------------------------------------------------

# Finderで隠しファイルを表示する
defaults write com.apple.finder AppleShowAllFiles TRUE

# マウスカーソル速度
defaults write -g com.apple.trackpad.scaling 5

# キーリピート
defaults write -g InitialKeyRepeat -int 15
defaults write -g KeyRepeat -int 2

# ----------------------------------------------------------------------
title "Utils"
# ----------------------------------------------------------------------

brew install coreutils findutils gnu-sed gnu-tar grep

# ----------------------------------------------------------------------
title "Install tmux"
# ----------------------------------------------------------------------

brew install tmux reattach-to-user-namespace
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
ln -s $DOTDIR/.tmux.conf ~/

# ----------------------------------------------------------------------
title "Setting zsh"
# ----------------------------------------------------------------------

ln -s $DOTDIR/zsh $XDG_CONFIG_HOME
ln -s $XDG_CONFIG_HOME/zsh/.zshenv ~/

# ----------------------------------------------------------------------
title "Install emacs"
# ----------------------------------------------------------------------

# todo: build

# ----------------------------------------------------------------------
title "Install karabiner-elements"
# ----------------------------------------------------------------------

brew install karabiner-elements
cp $DOTDIR/karabiner/assets/complex_modifications/alt2kana.json ~/.config/karabiner/assets/complex_modifications/alt2kana.json 

# ----------------------------------------------------------------------
title "done."
