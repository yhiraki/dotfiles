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
set nomore

" 描画の高速化
set nocursorcolumn
set nocursorline
set norelativenumber
" set synmaxcol=200
syntax sync minlines=256

" return to continue を表示しない
set shortmess=t

" ※等の記号を2バイト表示
set ambiwidth=double

" undoの永続化
if has('persistent_undo')
  set undodir=~/.vim/undo
  set undofile
endif

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
  if !has('nvim')
    set clipboard+=unnamed
  endif
  set clipboard+=unnamedplus
endif

" 行80, 100, 120+文字のラインを引く
if (exists('+colorcolumn'))
  let &colorcolumn="80,100,".join(range(120,999),",")
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

" " スペルチェックを ON
" set spell
" " 日本語をスペルチェックから除外
" set spelllang=en,cjk
"
" " スペルチェック highlight を下線のみに
" " http://tango-ruby.hatenablog.com/entry/2015/09/04/175729
" hi clear SpellBad
" hi clear SpellLocal
" hi clear SpellRare
" hi clear SpellCap
" hi SpellBad cterm=underline
