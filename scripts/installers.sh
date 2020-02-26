#!/usr/bin/env bash

install_essentials() {
  case "${OSNAME}" in
    osx) ;;
    linux) sudo apt-get install -y git ;;
  esac
}

install_emacs() {
  local version="$1"
  version="${version:=26.3}"
  case "${OSNAME}" in
    osx)
      # TODO: switch versions
      brew cask install emacs
      ;;
    linux)
      sudo add-apt-repository ppa:kelleyk/emacs
      case "${version}" in
        25*) sudo apt-get install -y emacs25=${version}* ;;
        26*) sudo apt-get install -y emacs26=${version}* ;;
        *) exit 1 ;;
      esac
      ;;
  esac
}
