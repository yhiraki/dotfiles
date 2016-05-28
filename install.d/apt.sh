#!/bin/bash

APT_INSTALL_LIST='
git
zsh
golang
'

for i in $APT_INSTALL_LIST; do
  sudo apt-get -y install $i
done
