# startコマンドで補完が効かない問題
if which start > /dev/null; then
  function mstart(){
    for arg in $@
    do
      start $arg
    done
  }
  alias start=mstart
fi
