augroup setMarkdownFiletype
  autocmd! FileType markdown setlocal shiftwidth=4 tabstop=4
augroup END

" .mdファイルをmarkdownに紐付け
au BufNewFile,BufRead *.md :set filetype=markdown
