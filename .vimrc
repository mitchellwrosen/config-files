set nocompatible
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'gmarik/Vundle.vim'
Plugin 'Shougo/vimproc.vim'
Plugin 'scrooloose/syntastic'
Plugin 'kien/ctrlp.vim'
Plugin 'vim-scripts/wombat256.vim'
Plugin 'godlygeek/tabular'
Plugin 'def-lkb/vimbufsync'
Plugin 'tomtom/tcomment_vim'
Plugin 'bling/vim-airline'

Plugin 'neovimhaskell/haskell-vim',  { 'for': 'haskell' }
Plugin 'eagletmt/ghcmod-vim',        { 'for': 'haskell' }
Plugin 'the-lambda-church/coquille', { 'for': 'coq'     }
Plugin 'idris-hackers/idris-vim',    { 'for': 'idris'   }

call vundle#end()

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
set shiftwidth=4
set showmatch
set smartcase
set smartindent
set smarttab
set softtabstop=4
set tabstop=4
set timeoutlen=1000
set ttimeoutlen=0
set whichwrap+=<,>,h,l
set wrap

if &listchars ==# 'eol:$'
  set listchars=tab:>\ ,trail:-,extends:>,precedes:<,nbsp:+
endif

if &term =~ '256color'
  " disable Background Color Erase (BCE) so that color schemes
  " render properly when inside 256-color tmux and GNU screen.
  " see also http://snk.tuxfamily.org/log/vim-256color-bce.html
  set t_ut=
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
nnoremap <silent> <leader>o :CtrlP<CR>
let g:ctrlp_max_files=0
let g:ctrlp_show_hidden=1
let g:ctrlp_custom_ignore = { 'dir': '\v[\/](.git|.cabal-sandbox|.stack-work)$', 'file': '\v\.(o|hi|beam|dyn_hi|dyn_o)$' }

" haskell-vim
let g:haskell_enable_quantification=1
let g:haskell_enable_recursivedo=1
let g:haskell_enable_arrowsyntax=1
let g:haskell_enable_pattern_synonyms=1
let g:haskell_enable_typeroles=1
let g:haskell_enable_static_pointers=1

" ghc-mod
nmap <silent> <leader>ht :GhcModType<CR>
nmap <silent> <leader>hT :GhcModTypeInsert<CR>

" syntastic
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
" let g:syntastic_check_on_wq = 0
let g:syntastic_haskell_checkers = []

" tcomment
nnoremap <silent> <leader>m :TComment<CR>
vnoremap <silent> <leader>m :TComment<CR>

" Strip trailing whitespace on save
fun! <SID>StripTrailingWhitespaces()
    let l = line(".")
    let c = col(".")
    %s/\s\+$//e
    call cursor(l, c)
endfun
autocmd BufWritePre * :call <SID>StripTrailingWhitespaces()
