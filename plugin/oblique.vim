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

let s:init      = 0
let s:match_ids = {}
let s:backward  = 0
let s:fuzzy     = 0
let s:searchpos = []
let s:offset    = ''
let s:matching  = ''

function! s:optval(key)
  return get(g:, 'oblique#'.a:key, s:DEFAULT[a:key])
endfunction

" Returns true only once
function! s:init()
  if s:init
    return 0
  else
    let s:init = 1
    return 1
  endif
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
  let prev = histget('/', -1)
  if pat == a:repeat && !empty(prev)
    let pat = prev
  elseif a:fuzzy
    let chars = map(split(pat, '.\zs'), 'escape(v:val, "\\[]^$.*")')
    let pat = join(
      \ extend(map(chars[0 : -2], 'v:val . "[^" .v:val. "]\\{-}"'),
      \ chars[-1:-1]), '')
  endif

  let [t, offset] = s:match1(a:pat, '\\\@<!'.a:repeat.'\([esb]\?[+-]\?[0-9]*\)$')
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
  let last = substitute(@/, '^\\V', '', '')
  let mlen = s:optval('min_length')
  if s:ok
    call setpos('.', s:searchpos)
    call winrestview(s:view)
    call s:apply_offset()
    call s:unfold()
    call s:set_autocmd()
    call s:highlight_current_match()
    silent! call repeat#set("\<Plug>(Oblique-Repeat)")
    if len(last) < mlen
      call histdel('/', -1)
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

function! s:finish_star()
  call s:revert_showcmd()
  call winrestview(s:view)
  call s:set_autocmd()
  call s:highlight_current_match()
  if len(s:star_word) < s:optval('min_length')
    call histdel('/', -1)
  endif
endfunction

function! s:prefix_for(pat)
  if !&ignorecase || (&smartcase && a:pat =~# '[A-Z]')
    let prefix = '\C'
  else
    let prefix = '\c'
  endif
  return prefix . '\%'.line('.').'l\%'.col('.').'c'
endfunction

function! s:matchadd(...)
  call s:clear_highlight()
  let s:match_ids[bufnr('%')] = call('matchadd', a:000)
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
  let s:pos = [line('.'), col('.')]
  augroup Oblique
    autocmd!
    autocmd CursorMoved <buffer> call s:on_cursor_moved(0)
    autocmd InsertEnter <buffer> call s:on_cursor_moved(1)
  augroup END
endfunction

function! s:clear_highlight()
  let bn = bufnr('%')
  if has_key(s:match_ids, bn)
    silent! call matchdelete(remove(s:match_ids, bn)))
  endif
endfunction

function! s:clear_autocmd()
  augroup Oblique
    autocmd!
  augroup END
endfunction

function! s:clear()
  call s:clear_highlight()
  call s:clear_autocmd()
endfunction

function! s:on_cursor_moved(force)
  if a:force || line('.') != s:pos[0] || col('.') != s:pos[1]
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

function! s:next(n, gv)
  call s:clear()
  if a:gv
    normal! gv
  endif
  try
    execute 'normal! '.a:n
    call s:highlight_current_match()
    call s:unfold()
    call s:set_autocmd()
  catch
    echohl Error
    echo substitute(v:exception, '.\{-}:', '', '')
    echohl None
  endtry
endfunction

function! s:oblique(gv, backward, fuzzy)
  let s:backward = a:backward
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

  normal! m`
  try
    let sym = s:backward ? '?' : '/'
    let opts = {
    \ 'prompt':    ['ObliquePrompt', (s:fuzzy ? 'F' : '') . sym],
    \ 'input':     vmagic,
    \ 'history':   history,
    \ 'highlight': 'ObliqueLine',
    \ 'on_change': function('g:_oblique_on_change')
    \ }

    if !a:fuzzy
      let opts.on_unknown_key = function('g:_oblique_on_unknown_key')
    endif

    let input = pseudocl#start(opts)
    let [@/, s:offset] = s:build_pattern(input, sym, s:fuzzy)
    if s:search(@/)
      let s:ok        = 1
      let s:view      = winsaveview()
      let s:searchpos = getpos('.')
      let s:input     = input
      keepjumps normal! ``
    else
      call pseudocl#render#clear()
      echohl Error
      echon 'E486: Pattern not found: '. @/
      echohl None
    endif
    return @/
  catch 'exit'
    call winrestview(s:oview)
    call pseudocl#render#clear()
    return @/
  finally
    call s:clear_highlight()
  endtry
endfunction

function! s:escape_star_pattern(pat, backward)
  return substitute(escape(a:pat, '\' . (a:backward ? '?' : '/')), "\n", '\\n', 'g')
endfunction

function! s:star_search(backward, gv)
  call s:clear_highlight()

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
    let pat = '\V\<' . s:escape_star_pattern(s:star_word, a:backward) . '\>'
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
  " <Plug> maps
  for fz in [0, 1]
    for bw in [0, 1]
      for m in ['n', 'o', 'v']
        execute printf(m.'noremap <silent> <Plug>(Oblique-%s) :<C-U>let @/ = <SID>oblique(%s, %d, %d)<BAR>'
          \ . 'if <SID>ok()<BAR>silent execute <SID>move(%d)<BAR>endif<BAR>call <SID>finish()<CR>',
          \ (fz ? 'F' : '') . (bw ? '?' : '/'),
          \ m == 'v', bw, fz, bw)
      endfor
    endfor
  endfor

  for [bw, cmd] in [[0, '*'], [1, '#']]
    for m in ['n', 'v']
      execute printf(m.'noremap <silent> <Plug>(Oblique-%s) :<C-U>let @/ = <SID>star_search(%d, %d)<BAR>'
        \ . 'if <SID>ok()<BAR>silent execute <SID>move(%d)<BAR>call <SID>finish_star()<BAR>endif<CR>',
        \ cmd, bw, m == 'v', bw)
    endfor
  endfor

  " Setup default maps
  for m in ['n', 'v', 'o']
    for d in ['/', '?']
      if !hasmapto('<Plug>(Oblique-'.d.')', m)
        execute m.'map '.d.' <Plug>(Oblique-'.d.')'
      endif
      if s:optval('enable_fuzzy_search') && !hasmapto('<Plug>(Oblique-F'.d.')', m)
        execute m.'map z'.d.' <Plug>(Oblique-F'.d.')'
      endif
    endfor
  endfor

  if s:optval('enable_star_search')
    for m in ['n', 'v']
      if !hasmapto('<Plug>(Oblique-*)', m)
        execute m."map * <Plug>(Oblique-*)"
      endif
      if !hasmapto('<Plug>(Oblique-#)', m)
        execute m."map # <Plug>(Oblique-#)"
      endif
    endfor
  endif

  nnoremap <silent> n :call <SID>next('n', 0)<BAR>if &hlsearch<BAR>set hlsearch<BAR>endif<cr>
  nnoremap <silent> N :call <SID>next('N', 0)<BAR>if &hlsearch<BAR>set hlsearch<BAR>endif<cr>
  vnoremap <silent> n :<c-u>call <SID>next('n', 1)<BAR>if &hlsearch<BAR>set hlsearch<BAR>endif<cr>
  vnoremap <silent> N :<c-u>call <SID>next('N', 1)<BAR>if &hlsearch<BAR>set hlsearch<BAR>endif<cr>

  nnoremap <silent> <Plug>(Oblique-Repeat) :call <SID>repeat()<CR>
endfunction

call s:define_maps()

let &cpo = s:cpo_save
unlet s:cpo_save

