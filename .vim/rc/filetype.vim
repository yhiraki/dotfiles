" markdown
augroup setMarkdownFiletype
  autocmd BufNewFile,BufRead *.md set filetype=markdown
augroup END

" conflu
autocmd BufNewFile,BufRead *.conflu set filetype=confluencewiki

" dbext result
autocmd BufNewFile,BufRead Result set filetype=dbext
