vim-oblique ![travis-ci](https://travis-ci.org/junegunn/vim-oblique.svg?branch=master)
===========

*Disclaimer: this plugin has many issues that cannot be easily fixed.
I suggest that you try simpler alternatives like
[vim-evanesco](https://github.com/pgdouyon/vim-evanesco) or
[vim-slash](https://github.com/junegunn/vim-slash).*

Improved `/`-search for Vim.

- Different highlight for the match under the cursor
- Automatically clears search highlight when cursor is moved
- Does not append short patterns to search history
- Readline key bindings
- Fuzzy-search
- Improved star-search (visual-mode, highlighting without moving)

Installation
------------

Use your favorite plugin manager. vim-oblique requires
[vim-pseudocl](https://github.com/junegunn/vim-pseudocl).

With [vim-plug](https://github.com/junegunn/vim-plug):

```vim
Plug 'junegunn/vim-pseudocl'
Plug 'junegunn/vim-oblique'
```

Usage
-----

vim-oblique overrides the following keys by default:

| Default Key | `<Plug>` map         | Description                                      |
| ----------- | -------------------- | ------------------------------------------------ |
| `/`         | `<Plug>(Oblique-/)`  | Forward search                                   |
| `?`         | `<Plug>(Oblique-?)`  | Backward search                                  |
| `z/`        | `<Plug>(Oblique-F/)` | Forward fuzzy-search                             |
| `z?`        | `<Plug>(Oblique-F?)` | Backward fuzzy-search                            |
| `n`         | `<Plug>(Oblique-n)`  | Repeat the last search                           |
| `N`         | `<Plug>(Oblique-N)`  | Repeat the last search in the opposite direction |
|             | `<Plug>(Oblique-n!)` | Repeat the last search (always forward)          |
|             | `<Plug>(Oblique-N!)` | Repeat the last search (always backward)         |
| `*`         | `<Plug>(Oblique-*)`  | Forward star-search (in normal and visual mode)  |
| `#`         | `<Plug>(Oblique-#)`  | Backward star-search (in normal and visual mode) |
| `g*`        | `<Plug>(Oblique-g*)` | Forward star-search (no word boundary match)     |
| `g#`        | `<Plug>(Oblique-g#)` | Backward star-search (no word boundary match)    |

(Unlike the default star-search, the overridden version will not move the cursor)

Customization
-------------

### Maps

Use the `<Plug>` maps in the above table to customize the maps.

### Options

- `g:oblique#min_length` (default: 3)
    - Patterns shorter than this will not be added to search history
- `g:oblique#incsearch_highlight_all` (default: 0)
    - To highlight all incremental matches (requires `incsearch`)
- `g:oblique#clear_highlight` (default: 1)
    - To clear search highlight or not
- `g:oblique#prefix` (default: '')
    - Option to prefix pattern with
- `g:oblique#enable_cmap` (default: 1)
    - Enable experimental cmap emulation

### Events

You can customize the behavior of vim-oblique by registering custom actions to
the following events of `User` group.

| Event           | When                 |
| --------------- | -------------------- |
| `Oblique`       | `/`, `?`, `z/`, `z?` |
| `ObliqueStar`   | `*`, `#`, `g*`, `g#` |
| `ObliqueRepeat` | `n`, `N`             |

The following example will move your cursor line to the middle of the screen
after search.

```vim
autocmd! User Oblique       normal! zz
autocmd! User ObliqueStar   normal! zz
autocmd! User ObliqueRepeat normal! zz
```

### Highlighting

Define the following highlight groups to change the color:

- `ObliquePrompt` (default: linked to `Label`)
- `ObliqueLine` (default: linked to `None`)
- `ObliqueCurrentMatch` (default: linked to `IncSearch`)
- `ObliqueCurrentIncSearch` (default: linked to `IncSearch`)

#### Example

```vim
hi! def link ObliqueCurrentMatch Keyword
hi! def link ObliquePrompt       Structure
hi! def link ObliqueLine         String
```

License
-------

MIT

