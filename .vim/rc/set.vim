set number
set incsearch
set autoindent
set ic
set shiftwidth=2
set tabstop=2
set expandtab
set laststatus=2
set t_Co=256
set background=dark
set hlsearch

" マーカーで閉じる
set foldtext=marker

" ペーストモードを簡単に切り替え
set pastetoggle=<f5>

" 0から始まる数値を10進数として処理する
set nrformats=

"ビープ音すべてを無効にする
set visualbell t_vb=
set noerrorbells "エラーメッセージの表示時にビープを鳴らさない"

if has('mouse')
  set mouse=a
endif

" 無名レジスタに入るデータを、*レジスタにも入れる。
if has('clipboard')
  set clipboard+=unnamed
endif

" 行80文字のラインを引く
if (exists('+colorcolumn'))
    set colorcolumn=80
    highlight ColorColumn ctermbg=9
endif

" 特殊文字の可視化
set list
set listchars=tab:>-,trail:-,extends:>,precedes:<,nbsp:%

" grep結果をQuickFixに表示
" http://qiita.com/yuku_t/items/0c1aff03949cb1b8fe6b
autocmd QuickFixCmdPost *grep* cwindow

if has('multi_byte_ime') || has('xim')
  " IME ON時のカーソルの色を設定(設定例:紫)
  highlight CursorIM guibg=Purple guifg=NONE
  " 挿入モード・検索モードでのデフォルトのIME状態設定
  set iminsert=0 imsearch=0
  if has('xim') && has('GUI_GTK')
    " XIMの入力開始キーを設定:
    " 下記の s-space はShift+Spaceの意味でkinput2+canna用設定
    "set imactivatekey=s-space
  endif
  " 挿入モードでのIME状態を記憶させない場合、次行のコメントを解除
  "inoremap <silent> <ESC> <ESC>:set iminsert=0<CR>
endif

" 全角スペースの可視化
" http://inari.hatenablog.com/entry/2014/05/05/231307
function! ZenkakuSpace()
    highlight ZenkakuSpace cterm=underline ctermfg=lightblue guibg=darkgray
endfunction

if has('syntax')
    augroup ZenkakuSpace
        autocmd!
        autocmd ColorScheme * call ZenkakuSpace()
        autocmd VimEnter,WinEnter,BufRead * let w:m1=matchadd('ZenkakuSpace', '　')
    augroup END
    call ZenkakuSpace()
endif
