" common
" http://secret-garden.hatenablog.com/entry/2015/09/28/000000

" 改行後に末尾のスペースを消す
call lexima#add_rule({
      \   "at" : '\S\+\s\+\%#$',
      \   "char" : "<CR>",
      \   "input" : "<ESC>diwo",
      \})

" ,の後にスペースを入れる
call lexima#add_rule({
      \   "at" : '\%#',
      \   "char" : ",",
      \   "input" : ",<Space>",
      \})

" ', ' の後にスペースを続けれないようにする
call lexima#add_rule({
      \   "at" : ', \%#',
      \   "char" : '<Space>',
      \   "input" : "",
      \})

" ', 'を一度に削除する
call lexima#add_rule({
      \   "at" : ', \%#',
      \   "char" : '<BS>',
      \   "input" : '<BS><BS>',
      \})

" =の前後にスペースを入れる
call lexima#add_rule({
      \   "at" : '\w\+\%#',
      \   "char" : "=",
      \   "input" : "<Space>=<Space>",
      \})

" =入力前にスペースがあったら入力しない
call lexima#add_rule({
      \   "at" : '\w\+ [-+\\*/%=]\=\%#',
      \   "char" : "=",
      \   "input" : "=<Space>",
      \})

" ' = 'の後ろにスペースをいれ続けない
call lexima#add_rule({
      \   "at" : '\w\+ =\+ \%#',
      \   "char" : "<Space>",
      \   "input" : "",
      \})

" ==の場合は1つスペースを消す
call lexima#add_rule({
      \   "at" : '\w\+ =\+ \%#',
      \   "char" : '=',
      \   "input" : "<BS>=<Space>",
      \})

" 演算子直後の=
call lexima#add_rule({
      \   "at" : '\w\+[-+\\*/%=]\%#',
      \   "char" : '=',
      \   "input" : "<Left> <Right>=<Space>",
      \})

" ' = 'を一度に消す
call lexima#add_rule({
      \   "at" : '\w\+ = \%#',
      \   "char" : '<BS>',
      \   "input" : "<BS><BS><BS>",
      \})

" ' == ', ' += '等を一度に消す
call lexima#add_rule({
      \   "at" : '\w\+ [-+\\*/%=]= \%#',
      \   "char" : '<BS>',
      \   "input" : "<BS><BS><BS><BS>",
      \})

" <BS>空白(インデント)を一気に削除
call lexima#add_rule({
      \   "at" : '^\s\+\%#$',
      \   "char" : '<BS>',
      \   "input" : "<ESC>kJDA",
      \})


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
      \   "at" : '\w\+(\n\=\([^)]\+\n\)*[^)]\+\%#',
      \   "char" : '=',
      \   "input" : "=",
      \   'filetype' : ['python'],
      \})

" ' = 'の後に改行したら'\'を入力
call lexima#add_rule({
      \   "at" : ' = \%#',
      \   "char" : '<CR>',
      \   "input" : '\<CR>',
      \   'filetype' : ['python'],
      \})

" クオート内改行では'\'を入力しない
call lexima#add_rule({
      \   "at" : "'.* = \%#'",
      \   "char" : '<CR>',
      \   "input" : '<CR>',
      \   'filetype' : ['python'],
      \})

" クオート内改行では'\'を入力しない
call lexima#add_rule({
      \   "at" : '".* = \%#"',
      \   "char" : '<CR>',
      \   "input" : '<CR>',
      \   'filetype' : ['python'],
      \})


" markdown

" 改行後に末尾のスペースが2つ以上の場合は2つ残す
call lexima#add_rule({
      \   "at" : '\S\+\s\+\%#$',
      \   "char" : "<CR>",
      \   "input" : "<ESC>ciw<Space><Space><CR>",
      \   'filetype' : ['markdown'],
      \})
