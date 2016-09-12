" http://d.hatena.ne.jp/osyo-manga/20120507/1336320335
let s:hook = {
\    "name" : "anim",
\    "kind" : "hook",
\    "index_counter" : 0,
\    "config" : {
\        "enable" : 0
\    }
\}

function! s:hook.on_ready(session, context)
    let self.index_counter = -2
endfunction

function! s:hook.on_output(session, context)
    let self.index_counter += 1
    if self.index_counter < 0
        return
    endif
    let anim_list = [
          \ '\',
          \ '|',
          \ '/',
          \ '-',
          \]
    echo  anim_list[ self.index_counter / 2 % len(anim_list)  ] . ' quickrun ...'
endfunction

call quickrun#module#register(s:hook, 1)
unlet s:hook


" http://qiita.com/Hatajoe/items/d07d788a9b101ea5c38d
if !exists("g:quickrun_config")
    let g:quickrun_config = {}
endif

"quickrun
let g:quickrun_config = get(g:, 'quickrun_config', {})

" quickrunのランナーにvimprocを使用する
let g:quickrun_config._ = {
\   'runner'    : 'vimproc',
\   'runner/vimproc/updatetime' : 60,
\   'outputter' : 'error',
\   'outputter/error/success' : 'buffer',
\   'outputter/error/error'   : 'buffer',
\   'outputter/buffer/split'  : ':rightbelow 8sp',
\   'outputter/buffer/close_on_empty' : 1,
\   'hook/anim/enable' : 1,
\}

" python 実行用の設定
let g:quickrun_config['python'] = {
\   'command' : 'python',
\}
let g:quickrun_config['python.pytest.doctest'] = {
\   'command' : 'py.test',
\   'cmdopt' : '--doctest-modules',
\   'tempfile': '$HOME/.vim/temp/__tmp__.py',
\}
let g:quickrun_config['python.nose.doctest'] = {
\   'command' : 'nosetests',
\   'cmdopt' : '--with-doctest',
\   'tempfile': '$HOME/.vim/temp/__tmp__.py',
\}
let g:quickrun_config['python.pytest'] = {
\   'command' : 'py.test',
\   'cmdopt' : '-s -v',
\   'tempfile' : '$HOME/.vim/temp/__tmp__.py',
\}

" Graphviz
let g:quickrun_config['dot'] = {
\ 'hook/cd/directory'              : '%S:p:h',
\ 'command'                        : 'dot',
\ 'cmdopt'                         : '',
\ 'exec'                           : '%c -T png %s -o %s:r.png',
\ 'outputter/quickfix/errorformat' : 'Error: %f: %m in line %l %.%#,%EError: %m,%C%m,%Z%m'
\}

let g:quickrun_config['plantuml'] = {
\  'command': 'plantuml'
\, 'exec': ['%c %s', 'open %s:p:r.png']
\, 'outputter': 'null'
\}

let g:quickrun_config["watchdogs_checker/_"] = {
      \ "outputter/quickfix/open_cmd" : "",
      \ "hook/qfstatusline_update/enable_exit" : 1,
      \ "hook/qfstatusline_update/priority_exit" : 4,
      \ }
