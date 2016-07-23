nnoremap <silent> <C-l> :<C-u>nohlsearch<CR>:<C-u>cclose<CR><C-l>

nnoremap <silent> <leader>bo :<C-u>NERDTree<CR>
nnoremap <silent> <leader>to :<C-u>TagbarOpen<CR>
nnoremap <silent> <leader>tc :<C-u>TagbarClose<CR>
nnoremap <silent> <leader>bc :<C-u>NERDTreeClose<CR>

nnoremap <silent> <leader>gs :<C-u> :Gstatus<CR>

nnoremap <silent> <leader>jo :<C-u> :JunkfileOpen<CR>

" コマンドライン履歴を一つ進む
cnoremap <C-n> <Down>
" コマンドライン履歴を一つ戻る
cnoremap <C-p> <Up>

" F5キーでコマンド履歴を開く
" F6キーで検索履歴を開く
:nnoremap <F5> <CR>q:
:nnoremap <F6> <CR>q/

" q:、q/、q? は無効化
:nnoremap q: <NOP>
:nnoremap q/ <NOP>

:" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap ga <Plug>(EasyAlign)

" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)nnoremap q? <NOP>
