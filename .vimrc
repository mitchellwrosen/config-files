set nocompatible        " disable vi-compatibility

" Setup Bundles/Runtime Path
filetype on
filetype off
"set rtp+=~/.vim/plugin
set rtp+=~/.vim/bundle/vundle
set rtp+=$GOROOT/misc/vim
call vundle#rc()

" Bundles
Bundle 'gmarik/vundle'

Bundle 'vim-scripts/a.vim'
Bundle 'wincent/Command-T'
Bundle 'vim-scripts/Conque-Shell'
Bundle 'godlygeek/csapprox'
"Bundle 'eagletmt/ghcmod-vim'
"Bundle 'ujihisa/neco-ghc'
Bundle 'Shougo/neocomplcache'
Bundle 'scrooloose/nerdcommenter'
Bundle 'vim-scripts/OmniCppComplete'
Bundle 'ervandew/supertab'
Bundle 'scrooloose/syntastic'
Bundle 'majutsushi/tagbar'
Bundle 'tomtom/tlib_vim'
Bundle 'MarcWeber/vim-addon-mw-utils'
Bundle 'Lokaltog/vim-powerline'
Bundle 'garbas/vim-snipmate'
Bundle 'tpope/vim-fugitive'
"Bundle 'Shougo/vimproc'
Bundle 'tpope/vim-surround'

" General
set background=dark         " Assume a dark background
filetype plugin indent on   " Automatically detect file types.
syntax on                   " syntax highlighting
scriptencoding utf-8

set laststatus=2           " always show the status line
set encoding=utf-8
set autowrite                   " automatically write a file when leaving a modified buffer
set shortmess+=aoOtTW           " abbrev. of messages (avoids 'hit enter')
set history=1000                " store a ton of history (default is 20)
set showcmd
set autoread
set hidden

set noswapfile
set nobackup
set nowritebackup
if has('persistent_undo')
   silent !mkdir ~/.vim/backups > /dev/null 2>&1
   set undodir=~/.vim/backups
   set undofile                " persistent undo is nice
   set undolevels=1000         " maximum number of changes that can be undone
   set undoreload=10000        " maximum number lines to save for undo on a buffer reload
endif

" Save/load view state (cursor position, etc)
au BufWinLeave *.* silent! mkview
au BufWinEnter *.* silent! loadview
" }

" UI {
colorscheme molokai

set backspace=indent,eol,start  " backspace for dummies
set linespace=0                 " No extra spaces between rows
"set number                      " Line numbers on
set foldenable
set cursorline
set showmatch                   " show matching brackets/parenthesis
set matchpairs+=<:>             " match < >
set hlsearch                    " highlight search terms
set winminheight=0              " windows can be 0 line high
set incsearch                   " incremental search
set wildmenu                    " show list instead of just completing
set wildmode=longest:full,full  " command <Tab> completion, list matches, then longest common part, then all.
set whichwrap=b,s,<,>,[,]       " backspace and cursor keys wrap to
set scrolljump=0                " lines to scroll when cursor leaves screen
set scrolloff=10                " minimum lines to keep above and below cursor
set sidescrolloff=5
set sidescroll=1
set list
set listchars=tab:::,trail:·,extends:#,nbsp:· " Highlight problematic whitespace, # on long lines
set textwidth=80
set colorcolumn=+1
hi ColorColumn ctermbg=black
" }

set nowrap                      " don't wrap long lines
set smartindent                 " indent at the same level of the previous line
set expandtab                   " tabs are spaces, not tabs
set softtabstop=3               " soft <Tab> is 3 spaces
set shiftwidth=3                " use indents of 3 spaces
set pastetoggle=<F12>           " pastetoggle (sane indentation on pastes)
"set comments=sl:/*,mb:*,elx:*/  " auto format comment blocks

" Remove trailing whitespaces and ^M chars on write.
au BufWritePre <buffer> :call setline(1,map(getline(1,"$"),'substitute(v:val,"\\s\\+$","","")'))

" Go-specific.
au FileType go au BufWritePre <buffer> silent Fmt
au FileType go setlocal listchars=tab:\ \ ,trail:.,extends:#,nbsp:.
au FileType go setlocal ts=4 sw=4 sts=4 noexpandtab
" }

au FileType python setlocal ts=4 sw=4 sts=4

" Key (re)Mappings {
let mapleader=','

nnoremap ; :

" Ugly vim latexsuite fix for <C-J> override
noremap <SID>__imap_jumpforward <Plug>IMAP_JumpForward

" Easier moving in tabs and windows
nnoremap <C-H> <C-W>h
nnoremap <C-J> <C-W>j
nnoremap <C-K> <C-W>k
nnoremap <C-L> <C-W>l
nnoremap <C-X> <C-W>x
nnoremap <C-C> <C-W>w
nnoremap <S-H> gT
nnoremap <S-L> gt

" Move locations with [l and ]l
nnoremap ]l :lnext<CR>
nnoremap [l :lprev<CR>

" Visual mode Ctrl+C copies to + buffer.
set clipboard=unnamedplus
vnoremap <C-C> "+y

" Stupid shift key fixes
if has("user_commands")
   command! -bang -nargs=* -complete=file E e<bang> <args>
   command! -bang -nargs=* -complete=file W w<bang> <args>
   command! -bang -nargs=* -complete=file Wq wq<bang> <args>
   command! -bang -nargs=* -complete=file WQ wq<bang> <args>
   command! -bang Wa wa<bang>
   command! -bang WA wa<bang>
   command! -bang Q q<bang>
   command! -bang QA qa<bang>
   command! -bang Qa qa<bang>
endif

" Yank from the cursor to the end of the line, to be consistent with C and D.
nnoremap Y y$

" Map Tab to write
nnoremap <Tab> :w<CR>

" Map ,Tab to write/quit
nnoremap <leader><Tab> :x<CR>
inoremap <leader><Tab> <Esc>:x<CR>

" Map ,q to write-quit all
nnoremap <leader>q :xa<CR>

" Map ,Q to quit all (no write)
nnoremap <leader>Q :qa!<CR>

" Map ,/ to clear highlighted search
nnoremap <silent> <CR> :nohlsearch<CR>

" Shortcuts
" Change working directory to that of the current file
cmap cwd lcd %:p:h
cmap cd. lcd %:p:h

" For when you forget to sudo.. really write the file.
cmap w!! w !sudo tee % >/dev/null

" Some helpers to edit mode
" http://vimcasts.org/e/14
cnoremap %% <C-R>=expand('%:h').'/'<CR>
map <leader>ew :e %%
map <leader>es :sp %%
map <leader>ev :vsp %%
map <leader>et :tabe %%
map <leader>eT :args * \| tab all<CR>

" Adjust viewports to the same size
map <leader>= <C-w>=

" Easier horizontal scrolling
map zl zL
map zh zH

" ,f formats in go files (necessary? I think I prefer :Fmt on write)
au FileType go nmap <leader>f :Fmt<CR>

" Plugins {
" a.vim {
map <leader>av :AV<CR>
map <leader>a<Space> :A<CR>
" }

" NeoComplCache {
let g:neocomplcache_enable_at_startup = 1
" }

" Powerline {
let g:Powerline_symbols = 'unicode'
" }

" Syntastic {
let g:syntastic_mode_map = { 'mode': 'active',
         \ 'passive_filetypes': ['java'] }

let g:syntastic_check_on_open=1
let g:syntastic_enable_signs=1
let g:syntastic_auto_loc_list=2
let g:syntastic_loc_list_height=5    " height of location list
let g:syntastic_error_symbol='✗'
let g:syntastic_warning_symbol='⚠'
let g:syntastic_stl_format = '[%E{Err: %fe #%e}%B{, }%W{Warn: %fw #%w}]'

let g:syntastic_cpp_check_header=0   " don't check headers from cpps
let g:syntastic_cpp_compiler_options=' -g -Wall -Wextra'

" Tagbar {
"au VimEnter * :TagbarOpen
"nnoremap <silent> <leader>t :TagbarOpen fj<CR>
" }
" }
" }
