#!/bin/bash

export GOPATH=$HOME
export PATH=$PATH:$GOPATH/bin

GO_INSTALL_LIST='
github.com/motemen/ghq
'

go get $GO_INSTALL_LIST
