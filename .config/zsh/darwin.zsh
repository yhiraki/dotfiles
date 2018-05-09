alias ls="ls -G"
alias ll="ls -lG"
alias la="ls -laG"
alias sed="gsed"
alias updatedb='nice -n 10 /usr/libexec/locate.updatedb'
alias locate="locate -d $FCODES"

# updatedb settings
mnt_list=($HOME/mnt/*)
export FCODES="$HOME/var/db/locate.database"
export LOCATE_PATH=$FCODES
export FILESYSTEMS="hfs ufs smbfs"
export PRUNEPATHS="/private/tmp /private/var/folders /private/var/tmp */Backups.backupdb */.git"
export SEARCHPATHS="/ $mnt_list"

mkdir -p $(dirname $FCODES)

last_update_path=/var/tmp/updatedb_last_attempt
time_past_updated=$(expr $(date +%s) - $(cat $last_update_path))
update_interval=$(expr 60 \* 60 \* 24)
if [ ! -f $last_update_path ] || [ $time_past_updated -gt $update_interval ]
then
  if echo $mnt_list | tr ' ' '\n' | head -1 | ls > /dev/null 2&>1
  then
    nohup "updatedb && date +%s > $last_update_path" > /dev/null &
    echo updatedb is started
  else
    date +%s > $last_update_path
    echo updatedb is postponed
  fi
fi
