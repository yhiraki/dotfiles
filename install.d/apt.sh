#!/bin/bash

APT_INSTALL_LIST=$(echo '
ctags
gawk
git
golang
vim
zsh
' | xargs)

sudo apt-get -y install $APT_INSTALL_LIST
