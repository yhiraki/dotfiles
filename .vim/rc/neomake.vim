" 保存時とenter時にNeomakeする
autocmd! BufWritePost,BufEnter * Neomake

let g:neomake_javascript_jshint_maker = {
    \ 'args': ['--verbose'],
    \ 'errorformat': '%A%f: line %l\, col %v\, %m \(%t%*\d\)',
    \ }
let g:neomake_javascript_enabled_makers = ['jshint']

let g:neomake_python_flake8_maker = {
    \ 'errorformat': '%A%f: line %l\, col %v\, %m \(%t%*\d\)',
    \ }
let g:neomake_python_enabled_makers = ['flake8']

let g:neomake_verbose=3
let g:neomake_logfile='/tmp/neomake_error.log'
