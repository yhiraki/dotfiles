let g:lightline = {
      \ 'colorscheme': 'hybrid',
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ],
      \             [ 'pyenv' ],
      \             [ 'fugitive', 'readonly', 'filename', 'modified' ] ],
      \   'right': [ [ 'qfstatusline', 'lineinfo' ],
      \             [ 'percent' ],
      \             [ 'fileformat', 'fileencoding', 'filetype' ] ],
      \ },
      \ 'component': {
      \   'readonly': '%{&readonly?"RO":""}',
      \   'modified': '%{&filetype=="help"?"":&modified?"+":&modifiable?"":"-"}',
      \   'fugitive': '%{exists("*fugitive#head")?fugitive#head():""}'
      \ },
      \ 'component_visible_condition': {
      \   'readonly': '(&filetype!="help"&& &readonly)',
      \   'modified': '(&filetype!="help"&&(&modified||!&modifiable))',
      \   'fugitive': '(exists("*fugitive#head") && ""!=fugitive#head())',
      \ },
      \ 'component_function': {
      \   'pyenv': 'LightLinePyenv'
      \ },
      \ 'component_expand': {
      \   'qfstatusline': 'qfstatusline#Update',
      \ },
      \ 'component_type': {
      \   'qfstatusline': 'error',
      \ },
      \ 'separator': { 'left': '', 'right': '' },
      \ 'subseparator': { 'left': '|', 'right': '|' }
      \ }

function! LightLinePyenv()
  if &filetype == "python"
    return pyenv#pyenv#get_activated_env()
  else
    return ""
  endif
endfunction
