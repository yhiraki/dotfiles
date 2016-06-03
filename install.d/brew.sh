#!/bin/bash

BREW_INSTALL_LIST=$(echo '
git
zsh
golang
autoconf
ctags
gdbm
gmp
gnutls
go
libevent
libtasn1
nettle
openssl
pcre
pkg-config
postgresql
pyenv
pyenv-virtualenv
readline
reattach-to-user-namespace
task
tmux
tree
vim
wget
' | xargs)

brew install $BREW_INSTALL_LIST
