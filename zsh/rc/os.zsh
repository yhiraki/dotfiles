case ${OSTYPE} in
  darwin*)
    EMACSCLIENT="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient"

    function emacsclient-gui() {
      local filename=$1
      bash -c "$EMACSCLIENT -c -a '' $filename &"
    }

    alias e='$EMACSCLIENT -nw -a ""'
    alias ge=emacsclient-gui
    ;;
  linux*) ;;

  msys*)
    if command -v start >/dev/null; then
      function mstart() {
        for arg in "$@"; do
          start "$arg"
        done
      }
      alias start=mstart
    fi
    ;;
esac
