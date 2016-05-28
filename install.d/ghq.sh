#!/bin/bash

GHQ_INSTALL_LIST='
junegunn/fzf
motemen/ghq
'

for p in $GHQ_INSTALL_LIST; do
  ghq get $p
done

$HOME/src/github.com/junegunn/fzf/install --all
