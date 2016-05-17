if empty(glob("~/.config/nvim/autoload/plug.vim"))
    execute '!curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs https://raw.github.com/junegunn/vim-plug/master/plug.vim'
endif

call plug#begin()

Plug 'Shougo/vimproc.vim', { 'do': 'make' }
" Plug 'benekastah/neomake'
Plug 'kien/ctrlp.vim'
Plug 'vim-scripts/wombat256.vim'
Plug 'godlygeek/tabular'
Plug 'def-lkb/vimbufsync'
Plug 'tomtom/tcomment_vim'
Plug 'bling/vim-airline'
Plug 'scrooloose/nerdtree'
Plug 'majutsushi/tagbar'

Plug 'Valloric/YouCompleteMe',
    \ {
    \   'do': './install.py',
    \   'for': 'haskell'
    \ }

Plug 'neovimhaskell/haskell-vim', { 'for': 'haskell' }
Plug 'eagletmt/ghcmod-vim',       { 'for': 'haskell' }
" Plug 'eagletmt/neco-ghc',         { 'for': 'haskell' }

Plug 'the-lambda-church/coquille', { 'for': 'coq'     }
Plug 'idris-hackers/idris-vim',    { 'for': 'idris'   }

Plug 'raichoo/purescript-vim'

call plug#end()

let mapleader = " "
let g:mapleader = " "

set autoread
set autoindent
set backspace=eol,start,indent
set cmdheight=1
set encoding=utf8
set expandtab
set ffs=unix,dos,mac
set formatoptions=croql
set guicursor=n-v-c:block-Cursor
set guicursor+=n-v-c:blinkon0
set hlsearch
set ignorecase
set incsearch
set laststatus=2
set lazyredraw
set list
set magic
set mat=2
set noswapfile
set number
set ruler
set scrolloff=5
set shiftwidth=2
set showmatch
set smartcase
set smartindent
set smarttab
set softtabstop=2
set tabstop=2
set tags=tags;/,codex.tags;/
set timeoutlen=1000
set ttimeoutlen=0
set whichwrap+=<,>,h,l
set wrap

if &listchars ==# 'eol:$'
  set listchars=tab:>\ ,trail:-,extends:>,precedes:<,nbsp:+
endif

if has("autocmd")
    au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
endif

syntax enable
filetype plugin on
filetype indent on

colorscheme wombat256mod

nnoremap <C-H> <C-W>h
nnoremap <C-J> <C-W>j
nnoremap <C-K> <C-W>k
nnoremap <C-L> <C-W>l

nnoremap <S-H> gT
nnoremap <S-L> gt

nnoremap j gj
nnoremap k gk

nnoremap <silent> <Enter> :nohlsearch<CR>

nnoremap ; :
nnoremap <silent> <Tab> :w<CR>

" tabular
nnoremap <silent> <leader>a :execute "Tabularize /" . expand("<cWORD>")<CR>

" Ctrl+P
nnoremap <silent> <leader>o :CtrlP .<CR>
let g:ctrlp_max_files=0
let g:ctrlp_show_hidden=1
let g:ctrlp_custom_ignore =
    \ {
    \   'dir': '\v[\/](.git|.cabal-sandbox|.stack-work)$',
    \   'file': '\v\.(o|hi|beam|dyn_hi|dyn_o)$'
    \ }

" haskell-vim
let g:haskell_enable_quantification=1
let g:haskell_enable_recursivedo=1
let g:haskell_enable_arrowsyntax=1
let g:haskell_enable_pattern_synonyms=1
let g:haskell_enable_typeroles=1
let g:haskell_enable_static_pointers=1

" neomake
" let g:neomake_haskell_enabled_makers = ['ghcmod']

" tcomment
nnoremap <silent> <leader>m :TComment<CR>
vnoremap <silent> <leader>m :TComment<CR>

" YouCompleteMe
let g:ycm_filetype_whitelist =
    \ {
    \     'haskell': 1
    \ }
inoremap <C-j> <C-n>
inoremap <C-k> <C-p>

" NERDTree
nnoremap <silent> <leader>n :NERDTreeToggle<CR>

" tagbar
nnoremap <silent> <leader>t :TagbarToggle<CR>
nnoremap <leader>tg :!codex update --force<CR>

let g:tagbar_type_haskell = {
    \ 'ctagsbin'  : 'hasktags',
    \ 'ctagsargs' : '-x -c -o-',
    \ 'kinds'     : [
        \  'm:modules:0:1',
        \  'd:data: 0:1',
        \  'd_gadt: data gadt:0:1',
        \  't:type names:0:1',
        \  'nt:new types:0:1',
        \  'c:classes:0:1',
        \  'cons:constructors:1:1',
        \  'c_gadt:constructor gadt:1:1',
        \  'c_a:constructor accessors:1:1',
        \  'ft:function types:1:1',
        \  'fi:function implementations:0:1',
        \  'o:others:0:1'
    \ ],
    \ 'sro'        : '.',
    \ 'kind2scope' : {
        \ 'm' : 'module',
        \ 'c' : 'class',
        \ 'd' : 'data',
        \ 't' : 'type'
    \ },
    \ 'scope2kind' : {
        \ 'module' : 'm',
        \ 'class'  : 'c',
        \ 'data'   : 'd',
        \ 'type'   : 't'
    \ }
\ }

" Strip trailing whitespace on save
fun! <SID>StripTrailingWhitespaces()
    let l = line(".")
    let c = col(".")
    %s/\s\+$//e
    call cursor(l, c)
endfun
autocmd BufWritePre * :call <SID>StripTrailingWhitespaces()
