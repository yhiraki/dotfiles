#!/bin/bash

export GOPATH=$HOME
export PATH=$PATH:$GOPATH/bin

GO_CMD='go get'

# install ghq
$GO_CMD github.com/motemen/ghq

# install lemonade
cd /tmp
wget https://github.com/pocke/lemonade/releases/download/v1.1.0/lemonade_linux_amd64.tar.gz
tar xvzf lemonade_linux_amd64.tar.gz
mv lemonade $GOPATH/bin
