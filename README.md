vim-oblique
===========

Improved `/`-search for Vim.

- Automatically clears search highlight when cursor is moved
- Does not append short patterns to search history
- Fuzzy search
- Readline key bindings

Installation
------------

Use your favorite plugin manager. vim-oblique requires
[vim-pseudocl](https://github.com/junegunn/vim-pseudocl).

With [vim-plug](https://github.com/junegunn/vim-plug):

```vim
Plug 'junegunn/vim-oblique'
```

Or with [Vundle](https://github.com/gmarik/vundle):

```vim
Bundle 'junegunn/vim-pseudocl'
Bundle 'junegunn/vim-oblique'
```

Usage
-----

vim-oblique overrides the following keys by default:

| Default Key | <Plug> map           | Description                                      |
| ----------- | -------------------- | ------------------------------------------------ |
| `/`         | `<Plug>(Oblique-/)`  | Forward search                                   |
| `?`         | `<Plug>(Oblique-?)`  | Backward search                                  |
| `<Leader>/` | `<Plug>(Oblique-F/)` | Forward fuzzy-search                             |
| `<Leader>?` | `<Plug>(Oblique-F?)` | Backward fuzzy-search                            |
| `*`         | `<Plug>(Oblique-*)`  | Forward star-search (in normal and visual mode)  |
| `#`         | `<Plug>(Oblique-#)`  | Backward star-search (in normal and visual mode) |

(Unlike the default star-search, the overridden version will not move the cursor)

Customization
-------------

### Maps

Use the <Plug> maps in the above table to customize the maps.

### Options

- `g:oblique#min_length` (default: 3)
    - Patterns shorter than this will not be added to search history
- `g:oblique#clear_highlight` (default: 1)
    - To clear search highlight or not
- `g:oblique#very_magic` (default: 0)
    - To start pattern with `\V` or not

### Highlighting

Define the following highlight groups to change the color:

- `ObliquePrompt` (default: linked to `Label`)
- `ObliqueLine` (default: linked to `None`)

#### Example

```vim
hi! def link ObliquePrompt Structure
hi! def link ObliqueLine   String
```

License
-------

MIT

