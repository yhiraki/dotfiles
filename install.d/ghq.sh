#!/bin/bash

GHQ_CMD='ghq get'

$GHQ_CMD junegunn/fzf
$GHQ_CMD lysyi3m/osx-terminal-themes
$GHQ_CMD motemen/ghq
$GHQ_CMD tmux-plugins/tpm
$GHQ_CMD yyuu/pyenv
$GHQ_CMD yyuu/pyenv-virtualenv
$GHQ_CMD vimpr/vimperator-colors
$GHQ_CMD jawshooah/pyenv-default-packages

# fzf init
$HOME/src/github.com/junegunn/fzf/install --all
