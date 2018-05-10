#!/usr/bin/env bash

set -eu

log_file=/tmp/updatedb.log
mnt_list=($HOME/mnt/*)

if echo $mnt_list | tr ' ' '\n' | head -1 | xargs ls > /dev/null 2>&1
then
  echo "start updatedb : $(date)" > $log_file
  /usr/libexec/locate.updatedb >> $log_file 2>&1
  echo "end   updatedb : $(date)" >> $log_file
else
  echo updatedb canceled
fi
