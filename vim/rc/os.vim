if has('win32unix') || has('win64unix') || has('unix')

  set backup
  set backupdir=/tmp
  set directory=/tmp
  set backupskip=/tmp/*,/private/tmp/*

  set encoding=utf-8

  if filereadable(expand('~/.vimrc.local'))
    source ~/.vimrc.local
  endif

endif


if has('win32') || has('win64')

  ""Using with Cygwin"
  " VIM faq-33.6
  " https://github.com/Shougo/shougo-s-github/blob/master/vim/rc/windows.rc.vim
  "set shellcmdflag=-c
  "set shellxquote=\"
  "set shell=bash.exe
  "set shellpipe=2>&1\|\ tee
  "set shellredir=>%s\ 2>&1
  "set grepprg=grep

  set backup
  set backupdir=%temp%
  set directory=%temp%
  set encoding=utf-8
  let $HOME="%userprofile%\\.vim"

  if filereadable(expand('c:/vim/vimrc.local'))
    source c:/vim/vimrc.local
  endif

endif
