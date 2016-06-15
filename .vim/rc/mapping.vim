nnoremap <silent> <C-l> :<C-u>nohlsearch<CR>:<C-u>cclose<CR><C-l>

nnoremap <silent> <leader>bo :<C-u> :TagbarOpen<CR>:<C-u> :NERDTree<CR>
nnoremap <silent> <leader>bc :<C-u> :TagbarClose<CR>:<C-u> :NERDTreeClose<CR>

nnoremap <silent> <leader>gs :<C-u> :Gstatus<CR>

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
:nnoremap q? <NOP>
