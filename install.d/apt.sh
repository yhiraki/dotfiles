#!/bin/bash

APT_CMD="apt-get install -y"

# common
$APT_CMD ctags gawk git golang tmux vim wget zsh

# build tools
$APT_CMD make build-essential

# python build
$APT_CMD libssl-dev zlib1g-dev libbz2-dev libreadline-dev libsqlite3-dev curl llvm libncurses5-dev
