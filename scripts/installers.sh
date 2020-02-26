install_emacs() {
  case "${OSNAME}" in
    osx) brew install emacs ;;
    linux) sudo apt-get install emacs ;;
  esac
}
