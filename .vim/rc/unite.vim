"インサートモードで開始
let g:unite_enable_start_insert=1
"ヒストリー/ヤンク機能を有効化
let g:unite_source_history_yank_enable =1

" " カレントディレクトリ
nnoremap <silent> <leader>fe :<C-u>UniteWithBufferDir -buffer-name=files file<CR>
"バッファと最近開いたファイル一覧を表示
nnoremap <silent> <leader>fm :<C-u>Unite<Space>buffer file_mru<CR>
"最近開いたディレクトリを表示
nnoremap <silent> <leader>fd :<C-u>Unite<Space>directory_mru<CR>
" "バッファを表示
" nnoremap <silent> [unite]b :<C-u>Unite<Space>buffer<CR>
" "レジストリを表示
" nnoremap <silent> [unite]r :<C-u>Unite<Space>register<CR>
" "タブを表示
" nnoremap <silent> [unite]t :<C-u>Unite<Space>tab<CR>
" "ヒストリ/ヤンクを表示
" nnoremap <silent> [unite]h :<C-u>Unite<Space>history/yank<CR>
" "outline
" nnoremap <silent> [unite]o :<C-u>Unite<Space>outline<CR>
" "file_rec:!
" nnoremap <silent> [unite]<CR> :<C-u>Unite<Space>file_rec:!<CR>
