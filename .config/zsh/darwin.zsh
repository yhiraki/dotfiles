alias ls="ls -G"
alias ll="ls -lG"
alias la="ls -laG"
alias sed="gsed"
alias updatedb='nice -n 10 /usr/libexec/locate.updatedb'

# updatedb settings
mnt_list=($HOME/mnt/*)
export FCODES="$HOME/var/db/locate.database"
export LOCATE_PATH=$FCODES
export FILESYSTEMS="hfs ufs smbfs"
export PRUNEPATHS="/private/tmp /private/var/folders /private/var/tmp */Backups.backupdb */.git"
export SEARCHPATHS="/ $mnt_list"
mkdir -p $(dirname $FCODES)

alias locate="locate -d $FCODES"
