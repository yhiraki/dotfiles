setup_env() {
  DEBIAN_FRONTEND=noninteractive

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
  export DEBIAN_FRONTEND
}

setup_package_manager() {
  case "${OSNAME}" in
    osx)
      /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
      ;;
    linux)
      sudo apt-get update
      sudo apt-get install -y software-properties-common
      ;;
  esac
}

setup() {
  setup_env
  setup_package_manager
}
