#!/bin/bash

APT_INSTALL_LIST=$(echo '
gawk
git
zsh
golang
' | xargs)

sudo apt-get -y install $APT_INSTALL_LIST
