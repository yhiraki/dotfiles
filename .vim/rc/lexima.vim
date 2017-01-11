" common
" http://secret-garden.hatenablog.com/entry/2015/09/28/000000

" 改行後に末尾のスペースを消す
call lexima#add_rule({
      \   "at" : '\S\+\s\+\%#$',
      \   "char" : "<CR>",
      \   "input" : "<ESC>diwa<CR>",
      \})
call lexima#add_rule({
      \   "at" : '\S\+\s\+\%#',
      \   "char" : "<CR>",
      \   "input" : "<ESC>diwi<CR>",
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
        \   'at' : c . ' \%#',
        \   'char' : '<BS>',
        \   'input' : '<BS><BS>',
        \})
endfor

" 連続して : を入力する場合はスペースを入れない
call lexima#add_rule({
      \   'at' : ': \%#',
      \   'char' : ':',
      \   'input' : '<Left>:<Right>',
      \})

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

" 行頭では : 直後にスペースを入れない
call lexima#add_rule({
      \   'at' : '^\s*\%#',
      \   'char' : ':',
      \   'input' : ':',
      \})

" =の前後にスペースを入れる
call lexima#add_rule({
      \   'at' : '\S\+\%#',
      \   'char' : '=',
      \   'input' : '<Space>=<Space>',
      \})

" =入力前にスペースがあったら入力しない
call lexima#add_rule({
      \   'at' : '[^=]\+ [-+\\*/%=]\=\%#',
      \   'char' : '=',
      \   'input' : '=<Space>',
      \})

" ' = 'の後ろにスペースをいれ続けない
call lexima#add_rule({
      \   'at' : '\S\+\s[-+\\*/%=]\==\+\s\%#',
      \   'char' : '<Space>',
      \   'input' : '',
      \})

" ==の場合は1つスペースを消す
call lexima#add_rule({
      \   'at' : '\S\+\s=\+\s\%#',
      \   'char' : '=',
      \   'input' : '<BS>=<Space>',
      \})

" 演算子直後の=
call lexima#add_rule({
      \   'at' : '\S\+[-+\\*/%=]\%#',
      \   'char' : '=',
      \   'input' : '<Left> <Right>=<Space>',
      \})

" ' = 'を一度に消す
call lexima#add_rule({
      \   'at' : '\s=\s\%#',
      \   'char' : '<BS>',
      \   'input' : '<BS><BS><BS>',
      \})

" ' == ', ' += '等を一度に消す
call lexima#add_rule({
      \   'at' : '\s[-+\\*/%=]=\s\%#',
      \   'char' : '<BS>',
      \   'input' : '<BS><BS><BS><BS>',
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

" (): を一度に消す
call lexima#add_rule({
      \   'at'       : '^\s*\%(\<def\>\|\<if\>\|\<for\>\|\<while\>\|\<class\>\|\<with\>\)\s*\w\+(\%#):$',
      \   'char'     : '<BS>',
      \   'input'    : '<BS><Del><Del>',
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

" postgres 変数宣言 ':='
call lexima#add_rule({
      \   'at' : '\w\+: \%#',
      \   'char' : '=',
      \   'input' : '<BS><Left> <Right>=<Space>',
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


" yaml

" 行頭の - 直後はスペースを開ける
call lexima#add_rule({
      \   'at' : '^ *\%#',
      \   'char' : '-',
      \   'input' : '-<Space>',
      \   'filetype' : ['yaml'],
      \})
call lexima#add_rule({
      \   'at' : '^ *-\+ \%#',
      \   'char' : '-',
      \   'input' : '<Left>-<Right>',
      \   'filetype' : ['yaml'],
      \})


" html, htmldjango

" タグ内では = の間にスペースを入れない
call lexima#add_rule({
      \   'at' : '<.* \w\+\%#.*>',
      \   'char' : '=',
      \   'input' : '=',
      \   'filetype' : ['html', 'htmldjango'],
      \})
call lexima#add_rule({
      \   'at' : '{%.* \w\+\%#.*%}',
      \   'char' : '=',
      \   'input' : '=',
      \   'filetype' : ['html', 'htmldjango'],
      \})

" {%  %} の補完
call lexima#add_rule({
      \   'at' : '{\%#}',
      \   'char' : '%',
      \   'input' : '%%<Left>',
      \   'filetype' : ['htmldjango'],
      \})
call lexima#add_rule({
      \   'at' : '{%\%#%}',
      \   'char' : '<Space>',
      \   'input' : '<Space><Space><Left>',
      \   'filetype' : ['htmldjango'],
      \})


" sh, zsh, yaml

" = の間にスペースを入れない
call lexima#add_rule({
      \   'at' : '\%#',
      \   'char' : '=',
      \   'input' : '=',
      \   'filetype' : ['sh', 'zsh', 'yaml'],
      \})
