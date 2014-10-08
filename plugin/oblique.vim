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

let s:DEFAULT = {
\ 'min_length':          3,
\ 'clear_highlight':     1,
\ 'very_magic':          0,
\ 'enable_star_search':  1,
\ 'enable_fuzzy_search': 1
\ }

let s:backward  = 0
let s:fuzzy     = 0
let s:offset    = ''
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

function! s:build_pattern(pat, repeat, fuzzy)
  let pat = a:pat
  let prev = empty(s:prev) ? histget('/', -1) : s:prev
  if (empty(pat) || pat == a:repeat) && !empty(prev)
    let pat = prev
  elseif a:fuzzy
    let chars = map(split(pat, '.\zs'), 'escape(v:val, "\\[]^$.*")')
    let pat = join(
      \ extend(map(chars[0 : -2], 'v:val . "[^" .v:val. "]\\{-}"'),
      \ chars[-1:-1]), '')
  endif

  let [t, offset] = s:match1(pat, '\\\@<!'.a:repeat.'\([esb]\?[+-]\?[0-9]*\)$')
  if t
    let pat = strpart(pat, 0, len(pat) - len(offset) - 1)
  endif

  return [pat, offset]
endfunction

function! s:search(pat)
  call winrestview(s:oview)
  if empty(a:pat)
    return 0
  else
    let ok = 0
    try
      let ok = search(a:pat, 'c'. (s:backward ? 'b' : ''))
    catch
      let ok = 0
    endtry
    return ok
  endif
endfunction

" :help search-offset
function! s:apply_offset()
  if empty(s:offset) || s:offset =~ '^[sb]$'
    return
  elseif s:offset == 'e'
    execute "normal! gn\<Esc>"
    return
  elseif s:offset == '+'
    normal! j0
    return
  elseif s:offset == '-'
    normal! k0
    return
  elseif s:offset =~ '^[sb]+$'
    normal! l
    return
  elseif s:offset =~ '^[sb]-$'
    normal! h
    return
  elseif s:offset =~ '^e+$'
    execute "normal! gn\<Esc>l"
    return
  elseif s:offset =~ '^e-$'
    execute "normal! gn\<Esc>h"
    return
  endif

  let [t, o] = s:match1(s:offset, '^+\?\([0-9]\+\)$')
  if t
    execute 'normal! '.o.'j0'
    return
  endif

  let [t, o] = s:match1(s:offset, '^-\([0-9]\+\)$')
  if t
    execute 'normal! '.o.'k0'
    return
  endif

  let [t, o] = s:match1(s:offset, '^[sb]+\?\([0-9]\+\)$')
  if t
    execute 'normal! '.o.'l'
    return
  endif

  let [t, o] = s:match1(s:offset, '^[sb]-\([0-9]\+\)$')
  if t
    execute 'normal! '.o.'h'
    return
  endif

  let [t, o] = s:match1(s:offset, '^e+\?\([0-9]\+\)$')
  if t
    execute "normal! gn\<Esc>".o.'l'
    return
  endif

  let [t, o] = s:match1(s:offset, '^e-\([0-9]\+\)$')
  if t
    execute "normal! gn\<Esc>".o.'h'
    return
  endif

  " TODO ;{pattern}  perform another search, see |//;|
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

function! s:finish()
  call s:revert_showcmd()
  let last = substitute(@/, '^\\[CZMV]', '', 'g')
  let mlen = s:optval('min_length')
  if s:ok
    let s:prev = @/
    call winrestview(s:view)
    call s:apply_offset()
    call s:unfold()
    call s:set_autocmd()
    call s:highlight_current_match()
    silent! call repeat#set("\<Plug>(Oblique-Repeat)")
    if len(last) < mlen
      call histdel('/', -1)
    endif
    silent! doautocmd User Oblique
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

function! s:finish_star()
  let s:prev = @/
  call s:revert_showcmd()
  call winrestview(s:view)
  call s:set_autocmd()
  call s:highlight_current_match()
  call s:echo_pattern('n')
  if len(s:star_word) < s:optval('min_length')
    call histdel('/', -1)
  endif
  silent! doautocmd User ObliqueStar
endfunction

function! s:prefix_for(pat)
  if !&ignorecase ||
        \ (&smartcase && substitute(a:pat, '^\\[CZMV]', '', 'g') =~# '[A-Z]')
    let prefix = '\C'
  else
    let prefix = '\c'
  endif
  return prefix . '\%'.line('.').'l\%'.col('.').'c'
endfunction

function! s:matchadd(...)
  call s:clear_highlight()
  let w:match_id = call('matchadd', a:000)
endfunction

function! g:_oblique_on_change(new, old, cursor)
  if getchar(1) != 0
    return
  endif

  call s:clear_highlight()
  let [pat, off] = s:build_pattern(a:new, s:backward ? '?' : '/', s:fuzzy)
  let pmatching = s:matching
  if s:search(pat)
    try
      call s:matchadd("IncSearch", s:prefix_for(pat) . pat)
    catch
      " Ignore error
    endtry
    let s:matching = pat
  else
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
  return [a:code, a:new, a:cursor]
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
    autocmd WinLeave    <buffer> call s:on_win_leave(expand('<abuf>'))
  augroup END
endfunction

function! s:on_win_leave(buf)
  call s:clear_highlight(a:buf)
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

function! s:clear_highlight(...)
  let bn = a:0 > 0 ? a:1 : bufnr('%')
  silent! call matchdelete(w:match_id)
endfunction

function! s:clear_autocmd()
  execute 'augroup Oblique'.bufnr('%')
    autocmd!
  augroup END
  execute 'augroup! Oblique'.bufnr('%')
endfunction

function! s:clear()
  call s:clear_highlight()
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

function! s:highlight_current_match()
  try
    call s:matchadd('ObliqueCurrentMatch', s:prefix_for(@/) . @/)
  catch
    " ignore error
  endtry
endfunction

function! s:echo_pattern(n)
  echon "\r\r"
  echohl ObliquePrompt
  let bw = (a:n ==# 'n' ? s:backward : !s:backward)
  if s:fuzzy
    echon bw ? 'F?' : 'F/'
    echohl ObliqueLine
    echon s:input
  else
    echon bw ? '?' : '/'
    echohl ObliqueLine
    echon @/
  endif
  echohl None
endfunction

function! s:e486_fuzzy()
  return 'E486: Fuzzy pattern not found: '. s:input
endfunction

function! s:next(n, cnt, gv)
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
    silent! doautocmd User ObliqueRepeat
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
  let s:backward = a:backward
  let was_fuzzy  = s:fuzzy
  let s:fuzzy    = a:fuzzy
  let s:ok       = 0
  let s:oview    = winsaveview()
  let s:offset   = ''
  let s:matching = ''

  if a:gv
    normal! gv
  endif

  let history = map(reverse(range(1, &history)), 'histget("/", -v:val)')
  let vmagic  = s:optval('very_magic') ? '\V' : ''

  try
    let sym = s:backward ? '?' : '/'
    let opts = {
    \ 'prompt':    ['ObliquePrompt', (s:fuzzy ? 'F' : '') . sym],
    \ 'input':     vmagic,
    \ 'history':   history,
    \ 'highlight': 'ObliqueLine'
    \ }
    if &incsearch
      let opts.on_change = function('g:_oblique_on_change')
      if !a:fuzzy
        let opts.on_unknown_key = function('g:_oblique_on_unknown_key')
      endif
    endif

    let s:input = pseudocl#start(opts)
    let [@/, s:offset] = s:build_pattern(s:input, sym, s:fuzzy)
    if s:search(@/)
      let s:ok        = 1
      let s:view      = winsaveview()
      call winrestview(s:oview)
    else
      call pseudocl#render#clear()
      echohl ErrorMsg
      if s:fuzzy
        echon s:e486_fuzzy()
      else
        echon 'E486: Pattern not found: '. @/
      endif
      echohl None
    endif
    return @/
  catch 'exit'
    call winrestview(s:oview)
    call pseudocl#render#clear()
    let s:fuzzy = was_fuzzy
    return @/
  finally
    call s:clear_highlight()
  endtry
endfunction

function! s:escape_star_pattern(pat, backward)
  return substitute(escape(a:pat, '\' . (a:backward ? '?' : '/')), "\n", '\\n', 'g')
endfunction

function! s:star_search(backward, word, gv)
  call s:clear_highlight()

  let s:backward = a:backward
  let s:fuzzy = 0
  let s:view = winsaveview()
  let s:ok = 1
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
  return "normal! ". (a:bw ? '?' : '/') . @/ . "\<CR>"
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
        \ . 'if <SID>ok()<BAR>silent execute <SID>move(%d)<BAR>call <SID>finish_star()<BAR>endif<CR>',
        \ cmd, bw, w, m == 'x', bw)
    endfor
  endfor

  " Setup default maps
  let enable_fuzzy_search = s:optval('enable_fuzzy_search')
  for m in ['n', 'x', 'o']
    for d in ['/', '?']
      if !hasmapto('<Plug>(Oblique-'.d.')', m)
        execute m.'map '.d.' <Plug>(Oblique-'.d.')'
      endif
      if enable_fuzzy_search && !hasmapto('<Plug>(Oblique-F'.d.')', m)
        execute m.'map z'.d.' <Plug>(Oblique-F'.d.')'
      endif
    endfor
  endfor

  if s:optval('enable_star_search')
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

  nnoremap <silent> <Plug>(Oblique-Repeat) :call <SID>repeat()<CR>
endfunction

call s:define_maps()

let &cpo = s:cpo_save
unlet s:cpo_save

