#!/bin/bash

BREW_INSTALL_LIST=$(echo '
git
zsh
golang
' | xargs)

brew install $BREW_INSTALL_LIST
