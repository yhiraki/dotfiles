#!/bin/bash

BASEURL=https://yhiraki.github.io/dotfiles
DOTFILES_REPO=yhiraki/dotfiles
XDG_CONFIG_HOME="$HOME/.config"

title() {
  echo
  echo --------------------------------------------------------------------------------
  echo "$*"
  echo --------------------------------------------------------------------------------
}

title "Prepare"
{
  mkdir "$XDG_CONFIG_HOME"
}

title "Xcode command line tools"
{
  xcode-select --install
}

title "Install brew"
command -v brew || {
  mkdir -p .local/homebrew
  cd .local || exit 1
  curl -L https://github.com/Homebrew/brew/tarball/master | tar xz --strip 1 -C homebrew
}

title "Git settings"
{
  touch "$HOME/.gitconfig"
  [ -f ~/.gitconfig.local ] || curl "$BASEURL/.gitconfig.local" -o ~/.gitconfig.local
  git config --global include.path "$HOME/.gitconfig.local"
}

title "Install ghq"
command -v ghq || {
  brew install ghq
  [ "$(ghq root)" == "${HOME}/src" ] || exit 1
}

title "Fetch repo"
{
  ghq get "$DOTFILES_REPO"
  DOTDIR="$HOME/src/github.com/$DOTFILES_REPO"
  [ -d "$DOTDIR" ] || exit 1

  if [ ! -L ~/.gitconfig.local ]; then
    rm ~/.gitconfig.local
    ln -s "$DOTDIR/.gitconfig.local" ~/
  fi
}

title "Install essentials"
{
  brew install \
    ag \
    direnv \
    fzf \
    gibo \
    gnupg \
    terminal-notifier \
    wget
}

title "gcc g++"
command -v gcc || {
  brew install gcc
  ln -s /usr/local/bin/gcc-8 ~/bin/gcc
}
command -v g++ || {
  brew install llvm
  ln -s /usr/local/bin/g++-8 ~/bin/g++
}
command -v gdb || {
  brew install gdb
}

title "Install plantuml"
command -v dot || {
  brew install graphviz
}
{
  LIB_JAVA_DIR=$HOME/lib/java
  PLANTUML_JAR=$LIB_JAVA_DIR/plantuml.jar

  if [ -f "$PLANTUML_JAR" ]; then
    mkdir -p "$LIB_JAVA_DIR"
    wget http://downloads.sourceforge.net/project/plantuml/plantuml.jar -O "$PLANTUML_JAR"

    ln -s "$DOTDIR/plantuml/" "$XDG_CONFIG_HOME"
  fi
}

title "Mac defaults"
{
  defaults write com.apple.finder AppleShowAllFiles TRUE

  # マウスカーソル速度
  defaults write -g com.apple.trackpad.scaling 5

  # キーリピート
  defaults write -g InitialKeyRepeat -int 15
  defaults write -g KeyRepeat -int 2
}

title "Install gnu utils"
{
  brew install coreutils findutils gnu-sed gnu-tar grep
}

title "Install tmux"
command -v tmux || {
  brew install tmux
  git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
  ln -s "$DOTDIR/.tmux.conf" ~/
}

title "Setting zsh"
{
  dst="$XDG_CONFIG_HOME/zsh/"
  mkdir -p "$dst"
  ln -s "$DOTDIR/zsh/".* "$dst"
  ln -s "$dst/.zshenv" ~/
  unset dst
}

title "Setting emacs"
{
  dst="$XDG_CONFIG_HOME/.emacs.d/"
  mkdir -p "$dst"
  ln -s "$DOTDIR/.emacs.d/"* "$dst"
  unset dst
}

title "Install karabiner-elements"
{
  brew ls --versions karabiner-elements --cask && exit
  brew install karabiner-elements --cask
  cp "$DOTDIR/karabiner/assets/complex_modifications/alt2kana.json" \
    ~/.config/karabiner/assets/complex_modifications/alt2kana.json
}

title "done."
