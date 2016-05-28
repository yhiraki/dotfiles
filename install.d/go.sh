#!/bin/bash

GOPATH=$HOME
GO_INSTALL_LIST='
github.com/motemen/ghq
'

go get $GO_INSTALL_LIST
