" http://qiita.com/Hatajoe/items/d07d788a9b101ea5c38d

"quickrun
let g:quickrun_config = get(g:, 'quickrun_config', {})

" quickrunのランナーにvimprocを使用する
let g:quickrun_config._ = {
\   'runner'    : 'vimproc',
\   'runner/vimproc/updatetime' : 60,
\   'outputter' : 'error',
\   'outputter/error/success' : 'quickfix',
\   'outputter/error/error'   : 'quickfix',
\   'outputter/buffer/split'  : ':rightbelow 8sp',
\   'outputter/buffer/close_on_empty' : 1,
\}

" python 実行用の設定
let g:quickrun_config['python'] = {
\   'command' : 'python',
\   'outputter/buffer/filetype' : 'python_result'
\}
let g:quickrun_config['python.pytest.doctest'] = {
\   'command' : 'py.test',
\   'cmdopt' : '--doctest-modules',
\   'tempfile': '$HOME/.vim/temp/__tmp__.py',
\   'outputter/buffer/filetype' : 'doctest_result'
\}
let g:quickrun_config['python.nose.doctest'] = {
\   'command' : 'nosetests',
\   'cmdopt' : '--with-doctest',
\   'tempfile': '$HOME/.vim/temp/__tmp__.py',
\   'outputter/buffer/filetype' : 'doctest_result'
\}
let g:quickrun_config['python.pytest'] = {
\   'command' : 'py.test',
\   'cmdopt' : '-s -v',
\   'tempfile' : '$HOME/.vim/temp/__tmp__.py',
\   'outputter/buffer/filetype' : 'pytest_result'
\}

" Graphviz
let g:quickrun_config['dot'] = {
\ 'hook/cd/directory'              : '%S:p:h',
\ 'command'                        : 'dot',
\ 'cmdopt'                         : '',
\ 'exec'                           : '%c -T png %s -o %s:r.png',
\ 'outputter/quickfix/errorformat' : 'Error: %f: %m in line %l %.%#,%EError: %m,%C%m,%Z%m'
\}
