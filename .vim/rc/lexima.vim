" common
" http://secret-garden.hatenablog.com/entry/2015/09/28/000000

" 改行後に末尾のスペースを消す
call lexima#add_rule({
      \   "at" : '\S\+\s\+\%#$',
      \   "char" : "<CR>",
      \   "input" : "<ESC>diwo",
      \})

for c in [',', ':']
  " 後にスペースを入れる
  call lexima#add_rule({
        \   'at' : '\%#',
        \   'char' : c,
        \   'input' : c . '<Space>',
        \})
  " 後にスペースを続けれないようにする
  call lexima#add_rule({
        \   'at' : c . ' \%#',
        \   'char' : '<Space>',
        \   'input' : '',
        \})
  " 一度に削除する
  call lexima#add_rule({
        \   'at' : '\w\+' . c . ' \%#',
        \   'char' : '<BS>',
        \   'input' : '<BS><BS>',
        \})
endfor

" FQDN っぽい場合は : 直後にスペースを入れない
call lexima#add_rule({
      \   'at' : '\(\w\+\.\)\+\w\+\%#',
      \   'char' : ':',
      \   'input' : ':',
      \})

" 時刻や日付っぽい場合は : 直後にスペースを入れない
call lexima#add_rule({
      \   'at' : '\d\+\%#',
      \   'char' : ':',
      \   'input' : ':',
      \})

" =の前後にスペースを入れる
call lexima#add_rule({
      \   'at' : '\w\+\%#',
      \   'char' : '=',
      \   'input' : '<Space>=<Space>',
      \})

" =入力前にスペースがあったら入力しない
call lexima#add_rule({
      \   'at' : '\w\+ [-+\\*/%=]\=\%#',
      \   'char' : '=',
      \   'input' : '=<Space>',
      \})

" ' = 'の後ろにスペースをいれ続けない
call lexima#add_rule({
      \   'at' : '\w\+ [-+\\*/%=]\==\+ \%#',
      \   'char' : '<Space>',
      \   'input' : '',
      \})

" ==の場合は1つスペースを消す
call lexima#add_rule({
      \   'at' : '\w\+ =\+ \%#',
      \   'char' : '=',
      \   'input' : '<BS>=<Space>',
      \})

" 演算子直後の=
call lexima#add_rule({
      \   'at' : '\w\+[-+\\*/%=]\%#',
      \   'char' : '=',
      \   'input' : '<Left> <Right>=<Space>',
      \})

" ' = 'を一度に消す
call lexima#add_rule({
      \   'at' : '\w\+ = \%#',
      \   'char' : '<BS>',
      \   'input' : '<BS><BS><BS>',
      \})

" ' == ', ' += '等を一度に消す
call lexima#add_rule({
      \   'at' : '\w\+ [-+\\*/%=]= \%#',
      \   'char' : '<BS>',
      \   'input' : '<BS><BS><BS><BS>',
      \})

" <BS>空白(インデント)を一気に削除
call lexima#add_rule({
      \   'at' : '^\s\+\%#$',
      \   'char' : '<BS>',
      \   'input' : '<ESC>kJDA',
      \})

" docstring の改行
for c in ['''', '"']
call lexima#add_rule({
      \   'at' : c . '\{3}.*\%#' . c . '\{3}',
      \   'char' : '<CR>',
      \   'input_after' : '<CR>',
      \})
endfor


" python
" http://qiita.com/hatchinee/items/c5bc19a656925ce33882

" classとかの定義時に:までを入れる
call lexima#add_rule({
      \   'at'       : '^\s*\%(\<def\>\|\<if\>\|\<for\>\|\<while\>\|\<class\>\|\<with\>\)\s*\w\+.*\%#',
      \   'char'     : '(',
      \   'input'    : '():<Left><Left>',
      \   'filetype' : ['python'],
      \   })

" すでに:がある場合は重複させない. (smartinputでは、atの定義が長いほど適用の優先度が高くなる)
call lexima#add_rule({
      \   'at'       : '^\s*\%(\<def\>\|\<if\>\|\<for\>\|\<while\>\|\<class\>\|\<with\>\)\s*\w\+.*\%#.*:',
      \   'char'     : '(',
      \   'input'    : '()<Left>',
      \   'filetype' : ['python'],
      \   })

" 末尾:の手前でも、エンターとか:で次の行にカーソルを移動させる
call lexima#add_rule({
      \   'at'       : '^\s*\%(\<def\>\|\<if\>\|\<for\>\|\<while\>\|\<class\>\|\<with\>\)\s*\w\+.*\%#:$',
      \   'char'     : ':',
      \   'input'    : '<Right><CR>',
      \   'filetype' : ['python'],
      \   })

call lexima#add_rule({
      \   'at'       : '^\s*\%(\<def\>\|\<if\>\|\<for\>\|\<while\>\|\<class\>\|\<with\>\)\s*\w\+.*\%#:$',
      \   'char'     : '<CR>',
      \   'input'    : '<Right><CR>',
      \   'filetype' : ['python'],
      \   })

" デフォルト引数の場合は=の前後にスペースを入力しない
call lexima#add_rule({
      \   'at' : '\w\+(\n\=\([^)]\+\n\)*[^)]\+\%#',
      \   'char' : '=',
      \   'input' : '=',
      \   'filetype' : ['python'],
      \})

" ' = 'の後に改行したら'\'を入力
call lexima#add_rule({
      \   'at' : ' = \%#',
      \   'char' : '<CR>',
      \   'input' : '\<CR>',
      \   'filetype' : ['python'],
      \})

" クオート内改行では'\'を入力しない
call lexima#add_rule({
      \   'at' : '''.* = \%#''',
      \   'char' : '<CR>',
      \   'input' : '<CR>',
      \   'filetype' : ['python'],
      \})

" クオート内改行では'\'を入力しない
call lexima#add_rule({
      \   'at' : '".* = \%#"',
      \   'char' : '<CR>',
      \   'input' : '<CR>',
      \   'filetype' : ['python'],
      \})


" markdown

" 改行後に末尾のスペースが2つ以上の場合は2つ残す
call lexima#add_rule({
      \   'at' : '\S\+\s\s\+\%#$',
      \   'char' : '<CR>',
      \   'input' : '<ESC>ciw<Space><Space><CR>',
      \   'filetype' : ['markdown'],
      \})

" コード PRE の改行
call lexima#add_rule({
      \   'at' : '```.*\%#```',
      \   'char' : '<CR>',
      \   'input_after' : '<CR>',
      \   'filetype' : ['markdown'],
      \})

" コード PRE shell の場合 プロンプト $ を自動入力
call lexima#add_rule({
      \   'at' : '```sh\n\(\n.*\)*\%#\(\n.*\)*```',
      \   'char' : '<CR>',
      \   'input' : '<CR>$<Space>',
      \   'filetype' : ['markdown'],
      \})

" コード PRE shell の場合 行頭スペースでプロンプト $ を自動入力
call lexima#add_rule({
      \   'at' : '```sh\(\n.*\)*\n\%#\(\n.*\)*```',
      \   'char' : '<Space>',
      \   'input' : '$<Space>',
      \   'filetype' : ['markdown'],
      \})

" コード PRE shell の場合 プロンプト $ を一括削除
call lexima#add_rule({
      \   'at' : '```sh\(\n.*\)*$ \%#\(\n.*\)*```',
      \   'char' : '<BS>',
      \   'input' : '<BS><BS>',
      \   'filetype' : ['markdown'],
      \})


" sql
" http://www.agtech.co.jp/html/v8manuals/sqlref/Sqlkword3.html#611355
let s:sql_keywords = [
      \ 'absolute', 'action', 'add', 'all', 'allocate', 'alter', 'and', 'any', 'are', 'as', 'asc', 'assertion', 'at', 'authorization', 'avg', 'begin', 'between', 'bit', 'bit_length', 'both', 'by', 'cascade', 'cascaded', 'case', 'cast', 'catalog', 'char', 'character', 'char_length', 'character_length', 'check', 'close', 'coalesce', 'collate', 'collation', 'column', 'commit', 'connect', 'connection', 'constraint', 'constraints', 'continue', 'convert', 'corresponding', 'count', 'create', 'cross', 'current', 'current_date', 'current_time', 'current_timestamp', 'current_user', 'cursor', 'date', 'day', 'deallocate', 'dec', 'decimal', 'declare', 'default', 'deferrable', 'deferred', 'delete', 'desc', 'describe', 'descriptor', 'diagnostics', 'disconnect', 'distinct', 'domain', 'double', 'drop', 'else', 'end', 'end-exec', 'escape', 'except', 'exception', 'exec', 'execute', 'exists', 'external', 'extract', 'false', 'fetch', 'first', 'float', 'for', 'foreign', 'found', 'from', 'full', 'get', 'global', 'go', 'goto', 'grant', 'group', 'having', 'hour', 'identity', 'immediate', 'in', 'indicator', 'initially', 'inner', 'input', 'insensitive', 'insert', 'int', 'integer', 'intersect', 'interval', 'into', 'is', 'isolation', 'join', 'key', 'language', 'last', 'leading', 'left', 'level', 'like', 'local', 'lower', 'mask', 'match', 'max', 'min', 'minute', 'module', 'month', 'names', 'national', 'natural', 'nchar', 'next', 'no', 'not', 'null', 'nullif', 'numeric', 'octet_length', 'of', 'on', 'only', 'open', 'option', 'or', 'order', 'outer', 'output', 'overlaps', 'pad', 'partial', 'position', 'precision', 'prepare', 'preserve', 'primary', 'prior', 'privileges', 'procedure', 'public', 'read', 'real', 'references', 'relative', 'restrict', 'revoke', 'right', 'rollback', 'rows', 'schema', 'scroll', 'second', 'section', 'select', 'session', 'session_user', 'set', 'size', 'smallint', 'some', 'space', 'sql', 'sqlcode', 'sqlerror', 'sqlstate', 'substring', 'sum', 'system_user', 'table', 'temporary', 'then', 'time', 'timestamp', 'timezone_hour', 'timezone_minute', 'to', 'trailing', 'transaction', 'translate', 'translation', 'trim', 'true', 'union', 'unique', 'unknown', 'update', 'upper', 'usage', 'user', 'using', 'value', 'values', 'varchar', 'varying', 'view', 'when', 'whenever', 'where', 'with', 'work', 'write', 'year', 'zone'
      \ ]

let s:sql_keywords_match = '\<' . join(s:sql_keywords, '\>\|\<') . '\>'

" 予約を大文字にする
call lexima#add_rule({
      \   'at' : '\%('.s:sql_keywords_match.'\)\%#',
      \   'char' : '<Space>',
      \   'input' : '<ESC>gUiwea<Space>',
      \   'filetype' : ['sql'],
      \})

" 予約を大文字にする 例: INT(10)
call lexima#add_rule({
      \   'at' : '\%('.s:sql_keywords_match.'\)(.*)\%#',
      \   'char' : '<Space>',
      \   'input' : '<ESC>F(<Left>gUiwf)a<Space>',
      \   'filetype' : ['sql'],
      \})


" vim

" 変数の場合は:の後にスペースを入れない
call lexima#add_rule({
      \   'at' : '^[gbwtslv]\%#',
      \   'char' : ':',
      \   'input' : ':',
      \   'filetype' : ['vim'],
      \})
call lexima#add_rule({
      \   'at' : ' [gbwtslv]\%#',
      \   'char' : ':',
      \   'input' : ':',
      \   'filetype' : ['vim'],
      \})

" set 系コマンドでは = の間にスペースを入れない
call lexima#add_rule({
      \   'at' : '^set.*\%#',
      \   'char' : '=',
      \   'input' : '=',
      \   'filetype' : ['vim'],
      \})


" sh, zsh

" = の間にスペースを入れない
call lexima#add_rule({
      \   'at' : '\%#',
      \   'char' : '=',
      \   'input' : '=',
      \   'filetype' : ['sh', 'zsh'],
      \})
