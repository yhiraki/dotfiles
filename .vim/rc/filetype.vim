" python
augroup setPythonFiletype
  autocmd! FileType python setlocal shiftwidth=4 tabstop=4
augroup END

" markdown
augroup setMarkdownFiletype
  autocmd! FileType markdown setlocal shiftwidth=4 tabstop=4
augroup END

autocmd BufNewFile,BufRead *.md set filetype=markdown

" conflu
autocmd BufNewFile,BufRead *.conflu set filetype=confluencewiki

" snip
autocmd! FileType neosnippet setlocal noexpandtab
