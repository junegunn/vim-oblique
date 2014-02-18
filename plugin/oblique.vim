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

if hlexists('ObliqueLine')
  hi def link ObliquePrompt ObliqueLine
else
  hi def link ObliqueLine None
  hi def link ObliquePrompt Label
endif

let s:DEFAULT  = {
\ 'min_length':      3,
\ 'clear_highlight': 1,
\ 'very_magic':      0
\ }

let s:init      = 0
let s:mid       = 0
let s:backward  = 0
let s:fuzzy     = 0
let s:searchpos = []

" Returns true only once
function! s:init()
  if s:init
    return 0
  else
    let s:init = 1
    return 1
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
  return pat
endfunction

function! s:search(pat)
  keepjumps normal! ``
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

function! s:finish()
  let last = substitute(@/, '^\\V', '', '')
  let mlen = get(g:, 'oblique#min_length', s:DEFAULT.min_length)
  if s:ok
    call setpos('.', s:searchpos)
    call winrestview(s:view)
    call s:set_autocmd()
    if len(last) < mlen
      call histdel('/', -1)
    endif
  else
    if len(last) >= mlen
      call histadd('/', @/)
    endif
  end
endfunction

function! s:finish_star()
  call winrestview(s:view)
  normal! ``
  call s:set_autocmd()
  if len(s:star_word) < get(g:, 'oblique#min_length', s:DEFAULT.min_length)
    call histdel('/', -1)
  endif
endfunction

function! s:clear_highlight()
  if s:mid > 0
    silent! call matchdelete(s:mid)
    let s:mid = 0
  endif
endfunction

function! g:_oblique_on_change(new, old, cursor)
  if getchar(1) != 0
    return
  endif

  call s:clear_highlight()
  let pat = s:build_pattern(a:new, s:backward ? '?' : '/', s:fuzzy)
  if s:search(pat)
    let prefix = pat =~# '[A-Z' ? '\C' : '\c'
    let prefix .= '\%'.line('.').'l\%'.col('.').'c'
    let s:mid = matchadd("IncSearch", prefix . pat)
  endif
  redraw
endfunction

function! s:set_autocmd()
  if !get(g:, 'oblique#clear_highlight', s:DEFAULT.clear_highlight)
    augroup Oblique
      autocmd!
    augroup END
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

function! s:on_cursor_moved(force)
  if a:force || line('.') != s:pos[0] || col('.') != s:pos[1]
    set nohlsearch " function-search-undo
    augroup Oblique
      autocmd!
    augroup END
    return 1
  else
    return 0
  endif
endfunction

function! s:next(n)
  augroup Oblique
    autocmd!
  augroup END
  execute 'normal! '.a:n
  call s:set_autocmd()
endfunction

function! s:oblique(gv, backward, fuzzy)
  let s:backward = a:backward
  let s:fuzzy = a:fuzzy
  let s:ok = 0

  if a:gv
    normal! gv
  endif

  let history = map(reverse(range(1, &history)), 'histget("/", -v:val)')
  let vmagic  = get(g:, 'oblique#very_magic', s:DEFAULT.very_magic) ? '\V' : ''

  normal! m`
  try
    let sym = s:backward ? '?' : '/'
    let input = pseudocl#start({
    \ 'prompt':    ['ObliquePrompt', (s:fuzzy ? 'F' : '') . sym],
    \ 'input':     vmagic,
    \ 'history':   history,
    \ 'highlight': 'ObliqueLine',
    \ 'on_change': function('g:_oblique_on_change')
    \ })

    let @/ = s:build_pattern(input, sym, s:fuzzy)
    if s:search(@/)
      let s:ok        = 1
      let s:view      = winsaveview()
      let s:searchpos = getpos('.')
      keepjumps normal! ``
    else
      echohl Error
      call pseudocl#render#clear()
      echon 'E486: Pattern not found: '. @/
    endif
    return @/
  catch 'exit'
    call pseudocl#render#clear()
    return @/
  finally
    call s:clear_highlight()
  endtry
endfunction

function! s:escape_star_pattern(pat, backward)
  return substitute(escape(a:pat, a:backward ? '?' : '/'), "\n", '\\n', 'g')
endfunction

function! s:star_search(backward, gv)
  let s:view = winsaveview()
  let s:ok = 1
  if a:gv
    let xreg = @x
    normal! gv"xy
    let s:star_word = @x
    let pat = '\V' . s:escape_star_pattern(s:star_word, a:backward)
    call setreg('x', xreg, 'c')
  else
    let s:star_word = expand('<cword>')
    let pat = '\V\<' . s:escape_star_pattern(s:star_word, a:backward) . '\>'
  endif

  let @/ = pat
  return pat
endfunction

function! s:ok()
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
      execute printf(m.'noremap <silent> <Plug>(Oblique-%s) m`:<c-u>let @/ = <SID>star_search(%d, %d)<BAR>'
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
      if !hasmapto('<Plug>(Oblique-F'.d.')', m)
        execute m.'map <Leader>'.d.' <Plug>(Oblique-F'.d.')'
      endif
    endfor
  endfor

  for m in ['n', 'v']
    if !hasmapto('<Plug>(Oblique-*)', m)
      execute m."map * <Plug>(Oblique-*)"
    endif
    if !hasmapto('<Plug>(Oblique-#)', m)
      execute m."map # <Plug>(Oblique-#)"
    endif
  endfor

  nnoremap <silent> n :call <SID>next('n')<BAR>if <SID>init()<BAR>set hlsearch<BAR>endif<cr>
  nnoremap <silent> N :call <SID>next('N')<BAR>if <SID>init()<BAR>set hlsearch<BAR>endif<cr>
endfunction

call s:define_maps()

let &cpo = s:cpo_save
unlet s:cpo_save

