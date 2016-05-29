#!/bin/bash

GHQ_INSTALL_LIST=$(echo '
junegunn/fzf
motemen/ghq
yyuu/pyenv
yyuu/pyenv-virtualenv
' | xargs)

for p in $GHQ_INSTALL_LIST; do
  ghq get $p
done

$HOME/src/github.com/junegunn/fzf/install --all
