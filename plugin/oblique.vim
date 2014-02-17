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

let s:mid      = 0
let s:backward = 0
let s:fuzzy    = 0

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

function! s:map_n_N(backward)
  if a:backward
    nnoremap <silent> N :normal! n<cr>:call <SID>set_autocmd()<cr>
    nnoremap <silent> n :normal! N<cr>:call <SID>set_autocmd()<cr>
  else
    nnoremap <silent> n :normal! n<cr>:call <SID>set_autocmd()<cr>
    nnoremap <silent> N :normal! N<cr>:call <SID>set_autocmd()<cr>
  endif
endfunction

function! s:search(pat)
  keepjumps normal! ``
  if empty(a:pat) || search(a:pat, 'c'. (s:backward ? 'b' : '')) == 0
    return 0
  else
    return 1
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
  set hlsearch
  redraw
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

function! s:slash(gv, backward, fuzzy)
  let s:backward = a:backward
  let s:fuzzy = a:fuzzy

  if a:gv
    normal! gv
  endif
  normal! m`

  let history = map(reverse(range(1, &history)), 'histget("/", -v:val)')
  let vmagic  = get(g:, 'oblique#very_magic', s:DEFAULT.very_magic) ? '\V' : ''

  try
    let sym = s:backward ? '?' : '/'
    let pat = pseudocl#start({
    \ 'prompt':    ['ObliquePrompt', (s:fuzzy ? 'F' : '') . sym],
    \ 'input':     vmagic,
    \ 'history':   history,
    \ 'highlight': 'ObliqueLine',
    \ 'on_change': function('g:_oblique_on_change')
    \ })

    let pat = s:build_pattern(pat, sym, s:fuzzy)
    if len(pat) >= get(g:, 'oblique#min_length', s:DEFAULT.min_length) + len(vmagic)
      call histadd('/', pat)
    endif

    let @/ = pat
    if s:search(pat)
      call s:set_autocmd()
      call s:map_n_N(s:backward)
    else
      echohl Error
      call pseudocl#render#clear()
      echon 'E486: Pattern not found: '. pat
    endif
    return pat
  catch 'exit'
    call pseudocl#render#clear()
    return @/
  finally
    call s:clear_highlight()
    redraw
  endtry
endfunction

function! s:star_search(backward, gv)
  if a:gv
    let xreg = @x
    normal! gv"xy
    let word = @x
    let pat = '\V' . @x
    call setreg('x', xreg, 'c')
  else
    let word = expand('<cword>')
    let pat = '\V\<' . word . '\>'
  endif

  let @/ = pat
  if len(word) >= get(g:, 'oblique#min_length', s:DEFAULT.min_length)
    call histadd('/', pat)
  endif
  call s:set_autocmd()
  call s:map_n_N(a:backward)
  return pat
endfunction

" <Plug> maps
for fz in [0, 1]
  for bw in [0, 1]
    let name = (fz ? 'F' : '') . (bw ? '?' : '/')
    execute printf('nnoremap <silent> <Plug>(Oblique-%s) :let @/ = <SID>slash(0, %d, %d)<CR>', name, bw, fz)
    execute printf('vnoremap <silent> <Plug>(Oblique-%s) :<C-U>let @/ = <SID>slash(1, %d, %d)<CR>', name, bw, fz)
    execute printf('onoremap <silent> <Plug>(Oblique-%s) :<C-U>let @/ = <SID>slash(0, %d, %d)<CR>', name, bw, fz)
    unlet name
  endfor
endfor

nnoremap <silent> <Plug>(Oblique-*) :let @/ = <SID>star_search(0, 0)<cr>
nnoremap <silent> <Plug>(Oblique-#) :let @/ = <SID>star_search(1, 0)<cr>

vnoremap <silent> <Plug>(Oblique-*) :<c-u>let @/ = <SID>star_search(0, 1)<cr>
vnoremap <silent> <Plug>(Oblique-#) :<c-u>let @/ = <SID>star_search(1, 1)<cr>

" Setup maps
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

call s:map_n_N(0)

let &cpo = s:cpo_save
unlet s:cpo_save

