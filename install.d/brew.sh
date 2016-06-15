#!/bin/bash

BREW_CMD="brew install"

# commons
$BREW_CMD git zsh golang ctags tmux tree vim wget

# python
$BREW_CMD pyenv pyenv-virtualenvjawshooah/pyenv/pyenv-default-packages

# neovim
$BREW_CMD neovim/neovim/neovim

# for dot / plantuml
$BREW_CMD graphviz
