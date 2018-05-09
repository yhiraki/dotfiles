#!/usr/bin/env bash

mnt_list=($HOME/mnt/*)

if echo $mnt_list | tr ' ' '\n' | head -1 | ls > /dev/null 2&>1
then
  updatedb && date +%s > $last_update_path > /dev/null
fi
