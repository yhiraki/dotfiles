" https://github.com/junegunn/fzf/wiki/Examples-(vim)


" Jump to tags
function! s:tags_sink(line)
  let parts = split(a:line, '\t\zs')
  let excmd = matchstr(parts[2:], '^.*\ze;"\t')
  execute 'silent e' parts[1][:-2]
  let [magic, &magic] = [&magic, 0]
  execute excmd
  let &magic = magic
endfunction

function! s:tags()
  if empty(tagfiles())
    echohl WarningMsg
    echom 'Preparing tags'
    echohl None
    call system('ctags -R')
  endif

  call fzf#run({
        \ 'source':  'cat '.join(map(tagfiles(), 'fnamemodify(v:val, ":S")')).
        \            '| grep -v ^!',
        \ 'options': '+m -d "\t" --with-nth 1,4.. -n 1 --tiebreak=index',
        \ 'down':    '40%',
        \ 'sink':    function('s:tags_sink')})
endfunction

command! FZFTags call s:tags()


" mru search
command! FZFMru call fzf#run({
      \ 'source':  'tail +2 $XDG_CACHE_HOME/neomru/file',
      \ 'sink':    'edit',
      \ 'options': '-m -x +s',
      \ 'down':    '40%' })

" function! s:all_files()
"   return extend(
"         \ filter(copy(v:oldfiles),
"         \        "v:val !~ 'fugitive:\\|NERD_tree\\|^/tmp/\\|.git/\\|/private/'"),
"         \ map(filter(range(1, bufnr('$')), 'buflisted(v:val)'), 'bufname(v:val)'))
" endfunction


" lines
function! s:line_handler(l)
  let keys = split(a:l, ':\t')
  exec 'buf' keys[0]
  exec keys[1]
  normal! ^zz
endfunction

function! s:buffer_lines()
  let res = []
  for b in filter(range(1, bufnr('$')), 'buflisted(v:val)')
    call extend(res, map(getbufline(b,0,"$"), 'b . ":\t" . (v:key + 1) . ":\t" . v:val '))
  endfor
  return res
endfunction

command! FZFLines call fzf#run({
      \   'source':  <sid>buffer_lines(),
      \   'sink':    function('<sid>line_handler'),
      \   'options': '--extended --nth=3..',
      \   'down':    '60%'
      \})


" FZF find
command! -nargs=1 FZFFind call fzf#run({
      \ 'source': 'find <q-args>',
      \ 'sink': 'e',
      \ 'down': '40%'
      \ })

" FZF locate
command! -nargs=1 FZFLocate call fzf#run(
      \ {'source': 'locate <q-args>', 'sink': 'e', 'options': '-m'})

" mappings
nnoremap <silent> <leader>fe :<C-u>FZFFind .<CR>
nnoremap <silent> <leader>fa :<C-u>FZFLocate 
nnoremap <silent> <leader>fm :<C-u>FZFMru<CR>
nnoremap <silent> <leader>fl :<C-u>FZFLines<CR>
nnoremap <silent> <leader>ft :<C-u>FZFTags<CR>
