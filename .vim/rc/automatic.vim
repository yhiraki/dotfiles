" g:automatic_config に対してどのような設定を行うのかを記述する
" "match" でどのウィンドウに対する設定かを指定し、
" "set" でその値を設定する
let g:automatic_config = [
\   {
\       "match" : {
\           "bufname" : '\[quickrun output\]',
\       },
\       "set" : {
\           "height" : 5,
\       }
\   },
\]
