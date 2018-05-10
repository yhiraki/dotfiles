#!/usr/bin/env bash

updatedb='nice -n 10 /usr/libexec/locate.updatedb'
log_file=/tmp/updatedb.log
mnt_list=$(find $HOME/mnt -maxdepth 1 -type d)

FCODES="$HOME/var/db/locate.database"
LOCATE_PATH=$FCODES
FILESYSTEMS="hfs ufs smbfs"
PRUNEPATHS="/Applications /System /private/tmp /private/var/folders /private/var/tmp */Backups.backupdb */.git */*.lnk"
SEARCHPATHS="/ $mnt_list"
mkdir -p $(dirname $FCODES)

for d in $mnt_list
do
  if ! find $d -maxdepth 1 > /dev/null 2>&1
  then
    echo could not access to dir: $d
    echo updatedb canceled
    exit 1
  fi
done

echo start updatedb
echo "start updatedb : $(date)" >> $log_file
$updatedb >> $log_file 2>&1
echo "end   updatedb : $(date)" >> $log_file
