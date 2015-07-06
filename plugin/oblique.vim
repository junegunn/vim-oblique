" Copyright (c) 2014 Junegunn Choi
"
" MIT License
"
" Permission is hereby granted, free of charge, to any person obtaining
" a copy of this software and associated documentation files (the
" "Software"), to deal in the Software without restriction, including
" without limitation the rights to use, copy, modify, merge, publish,
" distribute, sublicense, and/or sell copies of the Software, and to
" permit persons to whom the Software is furnished to do so, subject to
" the following conditions:
"
" The above copyright notice and this permission notice shall be
" included in all copies or substantial portions of the Software.
"
" THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
" EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
" MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
" NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
" LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
" OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
" WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

let s:cpo_save = &cpo
set cpo&vim

if !hlexists('ObliqueLine')
  hi def link ObliqueLine None
endif

if !hlexists('ObliquePrompt')
  hi def link ObliquePrompt Label
endif

if !hlexists('ObliqueCurrentMatch')
  hi def link ObliqueCurrentMatch IncSearch
endif

if !hlexists('ObliqueCurrentIncSearch')
  hi def link ObliqueCurrentIncSearch IncSearch
endif

let s:DEFAULT = {
\ 'min_length':              3,
\ 'incsearch_highlight_all': 0,
\ 'clear_highlight':         1,
\ 'very_magic':              0,
\ 'prefix':                  '',
\ 'enable_cmap':             1
\ }

let s:backward  = 0
let s:fuzzy     = 0
let s:matching  = ''
let s:prev      = ''

function! s:optval(key)
  return get(g:, 'oblique#'.a:key, s:DEFAULT[a:key])
endfunction

function! s:match1(expr, pat)
  let arr = matchlist(a:expr, a:pat)
  if empty(arr)
    return [0, '']
  else
    return [1, arr[1]]
  endif
endfunction

" TODO ;{pattern}  perform another search, see |//;|
function! s:split(repeat, pat)
  return matchlist(a:pat, '\(.\{-}\)\(\\\@<!'.a:repeat.'[esb]\?[+-]\?[0-9]*\)\?$')[1:2]
endfunction

function! s:build_pattern(pat, repeat, fuzzy)
  let [pat, offset] = s:split(a:repeat, a:pat)
  let prev = empty(s:prev) ? histget('/', -1) : s:prev

  if a:fuzzy
    let chars = map(split(pat, '.\zs'), 'escape(v:val, "\\[]^$.*")')
    let pat = join(
      \ extend(map(chars[0 : -2], 'v:val . "[^" .v:val. "]\\{-}"'),
      \ chars[-1:-1]), '')
  elseif empty(pat) && !empty(prev)
    let pat = prev
  endif

  let offset = offset[1:-1]
  if empty(offset)
    let pat = substitute(pat, '\\\@<!'.a:repeat, '\\'.a:repeat, 'g')
  endif
  return [pat, offset]
endfunction

function! s:search(pat)
  call winrestview(s:view)
  if empty(a:pat)
    return 0
  else
    try
      for i in range(1, s:count)
        if !search(a:pat, (i == 1 ? 'c' : ''). (s:backward ? 'b' : ''))
          return 0
        endif
        if i == 1
          let s:stay = s:view.lnum == line('.') && s:view.col == col('.') - 1
        endif
      endfor
      return 1
    catch
      return 0
    endtry
  endif
endfunction

function! s:revert_showcmd()
  if exists('s:showcmd')
    let &showcmd = s:showcmd
    unlet! s:showcmd
  endif
endfunction

function! s:unfold()
  if foldclosed('.') != -1
    normal! zo
  endif
endfunction

function! s:strip_extra(pat)
  return substitute(a:pat, '\\[CZMV]\c', '', 'g')
endfunction

function! s:finish()
  call s:revert_showcmd()
  call s:clear()
  silent! call matchdelete(w:incsearch_id)
  silent! call matchdelete(w:current_incsearch_id)
  let last = s:strip_extra(@/)
  let mlen = s:optval('min_length')
  if s:ok
    let s:prev = @/
    call winrestview(s:view)
    execute s:move(s:backward)
    call s:echo_pattern('n')
    call s:unfold()
    call s:set_autocmd()
    call s:highlight_current_match()
    silent! call repeat#set("\<Plug>(Oblique-Repeat)")
    if len(last) < mlen
      call histdel('/', -1)
    endif
    if exists('#User#Oblique')
      doautocmd <nomodeline> User Oblique
    endif
  else
    if len(last) >= mlen
      call histadd('/', @/)
    endif
  end
endfunction

function! s:repeat()
  execute printf("normal! %s%s%s\<CR>",
    \ v:operator, s:backward ? '?' : '/', s:input)
  silent! call repeat#set("\<Plug>(Oblique-Repeat)")
endfunction

function! s:finish_star(gv)
  let s:prev = @/
  call s:revert_showcmd()
  call winrestview(s:view)

  let under = a:gv || getline('.')[col('.') - 1] =~ '\k' ||
                    \ getline('.')[col('.') :-1] !~ '\k'
  try
    let ws = &wrapscan
    if s:count > 1
      execute 'normal! ' . (s:count - under) . 'n'
    endif
    if !ws
      set wrapscan
    endif
    let pos = winsaveview()
    if under
      normal! l
      execute 'keepjumps normal!' (s:backward ? 'n' : 'N')
    elseif s:count == 1
      execute 'keepjumps normal!' (s:backward ? 'N' : 'n')
    endif
    call s:highlight_current_match()
    call winrestview(pos)
    call s:set_autocmd()
    call s:echo_pattern('n')
    if len(s:star_word) < s:optval('min_length')
      call histdel('/', -1)
    endif
    if exists('#User#ObliqueStar')
      doautocmd <nomodeline> User ObliqueStar
    endif
  catch
    echohl ErrorMsg
    echo substitute(v:exception, '.\{-}:', '', '')
    echohl None
  finally
    if !ws
      set nowrapscan
    endif
  endtry
endfunction

function! s:prefix_for(pat, highlight_all)
  if a:pat =~? '\\c'
    let prefix = ''
  elseif !&ignorecase || (&smartcase && s:strip_extra(a:pat) =~# '[A-Z]')
    let prefix = '\C'
  else
    let prefix = '\c'
  endif
  return a:highlight_all ? prefix : prefix . '\%'.line('.').'l\%'.col('.').'c'
endfunction

function! s:matchadd(...)
  let match_id = call('matchadd', a:000)
  if a:1 ==# 'ObliqueCurrentMatch'
    silent! call matchdelete(w:current_match_id)
    let w:current_match_id = match_id
  elseif a:1 ==# 'IncSearch'
    silent! call matchdelete(w:incsearch_id)
    let w:incsearch_id = match_id
  elseif a:1 ==# 'ObliqueCurrentIncSearch'
    silent! call matchdelete(w:current_incsearch_id)
    let w:current_incsearch_id = match_id
  endif
endfunction

function! g:_oblique_on_change(new, old, cursor)
  if getchar(1) != 0
    return
  endif

  let [pat, _] = s:build_pattern(a:new, s:backward ? '?' : '/', s:fuzzy)
  let pmatching = s:matching
  let empty = empty(a:new) || empty(s:strip_extra(pat))
  if !empty && s:search(pat)
    call s:highlight_current_match('IncSearch', pat, s:optval('incsearch_highlight_all'))
    call s:highlight_current_match('ObliqueCurrentIncSearch', pat)
    let s:matching = pat
  else
    if empty
      call winrestview(s:view)
    endif
    silent! call matchdelete(w:incsearch_id)
    silent! call matchdelete(w:current_incsearch_id)
    let s:matching = ''
  endif
  if pmatching != s:matching
    redraw
  endif
endfunction

function! g:_oblique_on_unknown_key(code, new, cursor)
  if !empty(s:matching) && a:code == "\<C-L>"
    let [a,  b]  = [getreg('a'),     getreg('b')]
    let [at, bt] = [getregtype('a'), getregtype('b')]
    let slash    = @/
    try
      let @/ = s:matching
      normal! gn"ay
      normal! gnl"by
      let xtra = strpart(@b, len(@a))
      if !empty(xtra) && xtra != "\n"
        if xtra == (s:backward ? '?' : '/')
          let xtra = '\'.xtra
        elseif (&ignorecase || &smartcase) && s:matching !~# '[A-Z]'
          let xtra = tolower(xtra)
        endif
        let new = strpart(a:new, 0, a:cursor) . xtra . strpart(a:new, a:cursor)
        let cursor = a:cursor + len(xtra)
        return [g:pseudocl#CONTINUE, new, cursor]
      endif
    finally
      call setreg('a', a, at)
      call setreg('b', b, bt)
      let @/ = slash
    endtry
  endif
  return [g:pseudocl#CONTINUE, a:new, a:cursor]
endfunction

function! s:set_autocmd()
  if !s:optval('clear_highlight')
    call s:clear_autocmd()
    return
  endif
  if !&hlsearch
    set hlsearch
  endif
  let b:_oblique_pos = [line('.'), col('.')]
  execute 'augroup Oblique'.bufnr('%')
    autocmd!
    autocmd CursorMoved <buffer> call s:on_cursor_moved(0)
    autocmd InsertEnter <buffer> call s:on_cursor_moved(1)
    autocmd WinLeave    <buffer> call s:on_win_leave()
  augroup END
endfunction

function! s:on_win_leave()
  silent! call matchdelete(w:current_match_id)
  augroup ObliqueExtra
    autocmd!
    autocmd CursorMoved * call s:on_win_enter()
  augroup END
endfunction

function! s:on_win_enter()
  augroup ObliqueExtra
    autocmd!
    autocmd CursorMoved * if exists('b:_oblique_pos')
                       \|   call s:on_cursor_moved(0)
                       \| else
                       \|   set nohlsearch
                       \| endif
                       \| augroup ObliqueExtra
                       \|   execute 'autocmd!'
                       \| augroup END
                       \| augroup! ObliqueExtra
  augroup END
endfunction

function! s:clear_autocmd()
  execute 'augroup Oblique'.bufnr('%')
    autocmd!
  augroup END
  execute 'augroup! Oblique'.bufnr('%')
endfunction

function! s:clear()
  silent! call matchdelete(w:current_match_id)
  call s:clear_autocmd()
endfunction

function! s:on_cursor_moved(force)
  if a:force || !exists('b:_oblique_pos') || line('.') != b:_oblique_pos[0] || col('.') != b:_oblique_pos[1]
    set nohlsearch " function-search-undo
    call s:clear()
    return 1
  else
    return 0
  endif
endfunction

function! s:highlight_current_match(...)
  let group = a:0 > 0 ? a:1 : 'ObliqueCurrentMatch'
  let pat = a:0 > 1 ? a:2 : @/
  let highlight_all = a:0 > 2 && a:3
  silent! call s:matchadd(group, s:prefix_for(pat, highlight_all) . '\&' . pat)
endfunction

if exists("*strdisplaywidth")
  function! s:strwidth(str)
    return strdisplaywidth(a:str)
  endfunction
else
  function! s:strwidth(str)
    return len(a:str)
  endfunction
endif

function! s:echo_pattern(n)
  let xy = [&ruler, &showcmd]
  set noruler noshowcmd
  echon "\r\r"
  let bw = (a:n ==# 'n' ? s:backward : !s:backward)
  let sym = bw ? '?' : '/'
  let [prompt, str] = s:fuzzy ? ['F'.sym, s:input] : [sym, s:term]
  if !s:fuzzy && !empty(s:offset)
    let str .= sym . s:offset
  endif
  echohl ObliquePrompt | echon prompt
  let max_width = winwidth(winnr()) - 2
  if s:strwidth(prompt . str) > max_width
    echohl NonText | echon '..'
    let str = str[0 : (max_width - s:strwidth('..' . prompt))]
  endif
  echohl ObliqueLine | echon str | echohl None
  let [&ruler, &showcmd] = xy
endfunction

function! s:e486_fuzzy()
  return 'E486: Fuzzy pattern not found: '. s:input
endfunction

function! s:next(n, cnt, gv)
  let s:term = get(s:, 'term', @/)
  let s:offset = get(s:, 'offset', '')
  let s:count = get(s:, 'count', 1)
  call s:clear()
  if a:gv
    normal! gv
  endif
  try
    execute 'normal! '.a:cnt.a:n
    call s:highlight_current_match()
    call s:unfold()
    call s:set_autocmd()
    call s:echo_pattern(a:n)
    if exists('#User#ObliqueRepeat')
      doautocmd <nomodeline> User ObliqueRepeat
    endif
  catch
    let msg = substitute(v:exception, '.\{-}:', '', '')
    echohl ErrorMsg
    if s:fuzzy && msg =~ '^E486'
      echo s:e486_fuzzy()
    else
      echo msg
    endif
    echohl None
  endtry
endfunction

function! s:oblique(gv, backward, fuzzy)
  let s:count    = v:count1
  let s:backward = a:backward
  let was_fuzzy  = s:fuzzy
  let s:fuzzy    = a:fuzzy
  let s:ok       = 0
  let s:stay     = 0
  let s:view     = winsaveview()
  let s:matching = ''

  if a:gv
    normal! gv
  endif

  let history = map(reverse(range(1, &history)), 'histget("/", -v:val)')
  let vmagic  = s:optval('very_magic') ? '\V' : ''

  try
    let sym = s:backward ? '?' : '/'
    let opts = {
    \ 'prompt':    ['ObliquePrompt', (s:count > 1 ? s:count : '') . (s:fuzzy ? 'F' : '') . sym],
    \ 'input':     s:optval('prefix') . vmagic,
    \ 'history':   history,
    \ 'map':       s:optval('enable_cmap'),
    \ 'highlight': 'ObliqueLine'
    \ }
    if &incsearch
      let opts.on_change = function('g:_oblique_on_change')
      if !a:fuzzy
        let opts.on_unknown_key = function('g:_oblique_on_unknown_key')
      endif
    endif

    let s:input = pseudocl#start(opts)
    let [pat, offset] = s:build_pattern(s:input, sym, s:fuzzy)
    if s:search(pat)
      let s:ok = 1
      " FIXME: does not try to retain the position when offset part is given
      let s:stay = empty(offset) && s:stay
      let s:term = pat
      let s:offset = offset
    else
      call pseudocl#render#clear()
      echohl ErrorMsg
      if s:fuzzy
        echon s:e486_fuzzy()
      else
        echon 'E486: Pattern not found: '. pat.offset
      endif
      echohl None
    endif
    return pat
  catch 'exit'
    call pseudocl#render#clear()
    let s:fuzzy = was_fuzzy
    return @/
  finally
    call winrestview(s:view)
  endtry
endfunction

function! s:escape_star_pattern(pat, backward)
  return substitute(escape(a:pat, '\' . (a:backward ? '?' : '/')), "\n", '\\n', 'g')
endfunction

function! s:star_search(backward, word, gv)
  silent! call matchdelete(w:current_match_id)

  let s:count = v:count1
  let s:backward = a:backward
  let s:fuzzy = 0
  let s:view = winsaveview()
  let s:ok = 1
  let s:stay = 0
  if a:gv
    let [xreg, xregtype] = [getreg('x'), getregtype('x')]
    silent! normal! gv"xy
    let s:star_word = @x
    let pat = '\V' . s:escape_star_pattern(s:star_word, a:backward)
    call setreg('x', xreg, xregtype)
  else
    let s:star_word = expand('<cword>')
    let esc = s:escape_star_pattern(s:star_word, a:backward)
    if empty(esc)
      let s:ok = 0
      echohl WarningMsg | echo 'No string under cursor' | echohl None
      return @/
    endif
    let pat = (esc[0] =~ '\k' && a:word) ? ('\V\<' . esc . '\>') : ('\V' . esc)
  endif

  let s:term = pat
  let s:offset = ''
  return pat
endfunction

function! s:ok()
  if s:ok && &showcmd
    let s:showcmd = 1
    set noshowcmd
  endif
  return s:ok
endfunction

function! s:move(bw)
  let sym = (a:bw ? '?' : '/')
  let off = empty(s:offset) ? '' : (sym . s:offset)
  return "normal! ". s:count . sym . s:term . off . "\<CR>" . (s:stay ? 'N' : '')
endfunction

function! s:define_maps()
  silent! call pseudocl#nop()
  if !exists('*pseudocl#start')
    echoerr 'junegunn/vim-pseudocl not found'
    return
  endif

  " <Plug> maps
  for fz in [0, 1]
    for bw in [0, 1]
      for m in ['n', 'o', 'x']
        execute printf(m.'noremap <silent> <Plug>(Oblique-%s) :<C-U>let @/ = <SID>oblique(%s, %d, %d)<BAR>'
          \ . 'if <SID>ok()<BAR>silent execute <SID>move(%d)<BAR>endif<BAR>call <SID>finish()<CR>',
          \ (fz ? 'F' : '') . (bw ? '?' : '/'),
          \ m == 'x', bw, fz, bw)
      endfor
    endfor
  endfor

  for [bw, w, cmd] in [[0, 1, '*'], [1, 1, '#'], [0, 0, 'g*'], [1, 0, 'g#']]
    for m in ['n', 'x']
      if !w && m == 'x' | continue | endif
      execute printf(m.'noremap <silent> <Plug>(Oblique-%s) :<C-U>let @/ = <SID>star_search(%d, %d, %d)<BAR>'
        \ . 'if <SID>ok()<BAR>silent! execute <SID>move(%d)<BAR>call <SID>finish_star(%d)<BAR>endif<CR>',
        \ cmd, bw, w, m == 'x', bw, m == 'x')
    endfor
  endfor

  " Setup default maps
  for m in ['n', 'x', 'o']
    for d in ['/', '?']
      if !hasmapto('<Plug>(Oblique-'.d.')', m)
        execute m.'map '.d.' <Plug>(Oblique-'.d.')'
      endif
      if !hasmapto('<Plug>(Oblique-F'.d.')', m)
        execute m.'map z'.d.' <Plug>(Oblique-F'.d.')'
      endif
    endfor
  endfor

  for m in ['n', 'x']
    if !hasmapto('<Plug>(Oblique-*)', m)
      execute m."map * <Plug>(Oblique-*)"
    endif
    if !hasmapto('<Plug>(Oblique-#)', m)
      execute m."map # <Plug>(Oblique-#)"
    endif
  endfor
  if !hasmapto('<Plug>(Oblique-g*)', 'n')
    execute "nmap g* <Plug>(Oblique-g*)"
  endif
  if !hasmapto('<Plug>(Oblique-g#)', 'n')
    execute "nmap g# <Plug>(Oblique-g#)"
  endif

  nnoremap <silent> <Plug>(Oblique-n) :<c-u>call <SID>next('n', v:count1, 0)<BAR>if &hlsearch<BAR>set hlsearch<BAR>endif<cr>
  nnoremap <silent> <Plug>(Oblique-N) :<c-u>call <SID>next('N', v:count1, 0)<BAR>if &hlsearch<BAR>set hlsearch<BAR>endif<cr>
  xnoremap <silent> <Plug>(Oblique-n) :<c-u>call <SID>next('n', v:count1, 1)<BAR>if &hlsearch<BAR>set hlsearch<BAR>endif<cr>
  xnoremap <silent> <Plug>(Oblique-N) :<c-u>call <SID>next('N', v:count1, 1)<BAR>if &hlsearch<BAR>set hlsearch<BAR>endif<cr>
  for m in ['n', 'x']
    if !hasmapto('<Plug>(Oblique-n)', m)
      execute m."map n <Plug>(Oblique-n)"
    endif
    if !hasmapto('<Plug>(Oblique-N)', m)
      execute m."map N <Plug>(Oblique-N)"
    endif
  endfor

  if empty(maparg('gd', 'n'))
    nnoremap gd gd:normal *<cr>
  endif
  if empty(maparg('gD', 'n'))
    nnoremap gD gD:normal *<cr>
  endif
  if empty(maparg('gd', 'x'))
    xnoremap gd gd<esc>:normal *<cr>gv
  endif
  if empty(maparg('gD', 'x'))
    xnoremap gD gD<esc>:normal *<cr>gv
  endif

  nnoremap <silent> <Plug>(Oblique-Repeat) :call <SID>repeat()<CR>
endfunction

call s:define_maps()

let &cpo = s:cpo_save
unlet s:cpo_save

