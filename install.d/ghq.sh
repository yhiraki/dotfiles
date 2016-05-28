#!/bin/bash

GHQ_INSTALL_LIST='
junegunn/fzf
motemen/ghq
'

for p in $GHQ_INSTALL_LIST; do
  ghq get $p
done

source $HOME/src/github.com/junegunn/fzf/install
