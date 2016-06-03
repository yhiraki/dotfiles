" --------------------------------------------------
" python {{{

augroup setPythonFiletype
  autocmd! FileType markdown setlocal shiftwidth=4 tabstop=4
augroup END

" }}}


" --------------------------------------------------
" markdwon {{{

augroup setMarkdownFiletype
  autocmd! FileType markdown setlocal shiftwidth=4 tabstop=4
augroup END

" .mdファイルをmarkdownに紐付け
au BufNewFile,BufRead *.md :set filetype=markdown

"}}}


"" --------------------------------------------------
" confluence {{{

" .mdファイルをmarkdownに紐付け
au BufNewFile,BufRead *.conflu :set filetype=confluencewiki

" }}}

