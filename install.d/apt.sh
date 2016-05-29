#!/bin/bash

APT_INSTALL_LIST=(
gawk
git
zsh
golang
)

sudo apt-get -y install $APT_INSTALL_LIST
