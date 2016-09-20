nnoremap <silent> <C-l> :<C-u>nohlsearch<CR>:<C-u>cclose<CR><C-l>

nnoremap <silent> <leader>bo :<C-u>NERDTree<CR>
nnoremap <silent> <leader>bc :<C-u>NERDTreeClose<CR>

nnoremap <silent> <leader>to :<C-u>TagbarOpen<CR>
nnoremap <silent> <leader>tc :<C-u>TagbarClose<CR>

nnoremap <silent> <leader>gs :<C-u> :Gstatus<CR>

nnoremap <silent> <leader>jo :<C-u> :JunkfileOpen<CR>
nnoremap <silent> <leader>jd :<C-u> :JunkfileDiary<CR>

nnoremap <silent> <leader>td :<C-u> :e ~/todo.txt<CR>

" コマンドライン履歴を一つ進む
cnoremap <C-n> <Down>
" コマンドライン履歴を一つ戻る
cnoremap <C-p> <Up>

" q:、q/、q? は無効化
:nnoremap q: <NOP>
:nnoremap q/ <NOP>

" quickrun ウインドウを閉じる
nnoremap <silent> <Leader>q :<C-u>bw! \[quickrun\ output\]<CR>
