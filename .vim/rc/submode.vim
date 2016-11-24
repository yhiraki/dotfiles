" submode.vim
" http://d.hatena.ne.jp/thinca/20130131/1359567419
" ウィンドウサイズの変更キーを簡易化する
call submode#enter_with('winsize', 'n', '', '<C-w>L', '<C-w>>')
call submode#enter_with('winsize', 'n', '', '<C-w>H', '<C-w><')
call submode#enter_with('winsize', 'n', '', '<C-w>J', '<C-w>-')
call submode#enter_with('winsize', 'n', '', '<C-w>K', '<C-w>+')
call submode#map('winsize', 'n', '', 'L', '<C-w>>')
call submode#map('winsize', 'n', '', 'H', '<C-w><')
call submode#map('winsize', 'n', '', 'J', '<C-w>-')
call submode#map('winsize', 'n', '', 'K', '<C-w>+')
