setup_env() {
  _ostype() {
    case $(uname | tr '[:upper:]' '[:lower:]') in
      linux*) echo linux ;;
      darwin*) echo osx ;;
      msys*) echo windows ;;
      *) echo notset ;;
    esac
  }
  OSNAME="$(_ostype)"
  unset -f _ostype

  export OSNAME
}

setup_package_manager() {
  case "${OSNAME}" in
    osx) /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)" ;;
    linux) sudo apt-get update ;;
  esac
}

setup () {
  setup_env
  setup_package_manager
}
