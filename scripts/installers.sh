#!/usr/bin/env bash

install_emacs() {
  local version="$1"
  version="${version:=26.3}"
  case "${OSNAME}" in
  osx)
    brew cask install emacs@${version}
    ;;
  linux)
    sudo add-apt-repository ppa:kelleyk/emacs
    case "${version}" in
      25*) apt-get install -y emacs25=${version}* ;;
      26*) apt-get install -y emacs26=${version}* ;;
      *) exit 1
    esac
    ;;
  esac
}
