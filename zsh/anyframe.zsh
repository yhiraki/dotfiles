HELPDIR=$HOME/.help

function my-manual(){
  if [ $# -eq 0 ]; then
    HELPFILE=${HELPDIR}/$(ls $HELPDIR | anyframe-selector-auto)
  else
    HELPFILE=${HELPDIR}/$1
  fi
  SELECTED_LINE=$(
    echo $(cat $HELPFILE \
      | sed '/^#.*/d' \
      | sed '/^$/d' \
      | fzf-tmux -q ${@:2:($#-1)}) \
      | sed -e 's/ *\[.*\] *//g'
  )
  if [ ${#$(echo $SELECTED_LINE | grep '#.*insert')} -ne 0 ]; then
    CMD=anyframe-action-insert
  else
    CMD=anyframe-action-execute
  fi
  echo -E $SELECTED_LINE \
    | $CMD
}

alias ma='my-manual'
