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

" quickrun ウインドウを閉じる
nnoremap <silent> <Leader>q :<C-u>bw! \[quickrun\ output\]<CR>
" 実行中の quickrun を中断させる
nnoremap <expr><silent> <C-c> quickrun#is_running() ? quickrun#sweep_sessions() : "\<C-c>"

" tags が複数ある時は一覧表示
nnoremap <C-]> g<C-]>
