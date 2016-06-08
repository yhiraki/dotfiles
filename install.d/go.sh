#!/bin/bash

export GOPATH=$HOME
export PATH=$PATH:$GOPATH/bin

GO_CMD='go get'

$GO_CMD github.com/motemen/ghq
