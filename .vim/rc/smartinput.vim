" common

" call smartinput#define_rule({
"       \   'at'       : '{\%#}',
"       \   'char'     : '<CR>',
"       \   'input'    : '<CR><CR><UP><TAB>',
"       \   'filetype' : ['python'],
"       \   })


" python
" http://qiita.com/hatchinee/items/c5bc19a656925ce33882

" classとかの定義時に:までを入れる
call smartinput#define_rule({
      \   'at'       : '^\s*\%(\<def\>\|\<if\>\|\<for\>\|\<while\>\|\<class\>\|\<with\>\)\s*\w\+.*\%#',
      \   'char'     : '(',
      \   'input'    : '():<Left><Left>',
      \   'filetype' : ['python'],
      \   })

" すでに:がある場合は重複させない. (smartinputでは、atの定義が長いほど適用の優先度が高くなる)
call smartinput#define_rule({
      \   'at'       : '^\s*\%(\<def\>\|\<if\>\|\<for\>\|\<while\>\|\<class\>\|\<with\>\)\s*\w\+.*\%#.*:',
      \   'char'     : '(',
      \   'input'    : '()<Left>',
      \   'filetype' : ['python'],
      \   })

" 末尾:の手前でも、エンターとか:で次の行にカーソルを移動させる
call smartinput#define_rule({
      \   'at'       : '^\s*\%(\<def\>\|\<if\>\|\<for\>\|\<while\>\|\<class\>\|\<with\>\)\s*\w\+.*\%#:$',
      \   'char'     : ':',
      \   'input'    : '<Right><CR>',
      \   'filetype' : ['python'],
      \   })

call smartinput#define_rule({
      \   'at'       : '^\s*\%(\<def\>\|\<if\>\|\<for\>\|\<while\>\|\<class\>\|\<with\>\)\s*\w\+.*\%#:$',
      \   'char'     : '<CR>',
      \   'input'    : '<Right><CR>',
      \   'filetype' : ['python'],
      \   })
