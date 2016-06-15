#!/bin/bash

APT_CMD="sudo apt-get install -y"

# common
$APT_CMD ctags gawk git golang tmux vim wget zsh

# build tools
$APT_CMD make build-essential

# python build
$APT_CMD libssl-dev zlib1g-dev libbz2-dev libreadline-dev libsqlite3-dev curl llvm libncurses5-dev

# for dot / plantuml
$APT_CMD graphviz

# neovim
$APT_CMD software-properties-common
sudo add-apt-repository -y ppa:neovim-ppa/unstable
sudo apt-get update
$APT_CMD neovim

# locale
$APT_CMD language-pack-ja
sudo update-locale LANG=ja_JP.UTF-8

# direnv
curl -L -o direnv https://github.com/zimbatm/direnv/releases/download/v2.5.0/direnv.linux-amd64
chmod 775 ./direnv
mv ./direnv $HOME/bin/direnv
