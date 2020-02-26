#!/usr/bin/env bash

_ostype() {
  case $(uname | tr '[:upper:]' '[:lower:]') in
    linux*)
      echo linux
      ;;
    darwin*)
      echo osx
      ;;
    msys*)
      echo windows
      ;;
    *)
      echo notset
      ;;
  esac
}
OSNAME="$(_ostype)"
unset -f _ostype
