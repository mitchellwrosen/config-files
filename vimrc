set nocompatible        " disable vi-compatibility

" Setup Bundles/Runtime Path {
filetype on
filetype off
set rtp+=~/.vim/plugin
set rtp+=~/.vim/bundle/vundle
set rtp+=$GOROOT/misc/vim
call vundle#rc()
" }

" Bundles {
Bundle 'gmarik/vundle'

Bundle 'godlygeek/csapprox'
Bundle 'scrooloose/nerdcommenter'
Bundle 'ervandew/supertab'
Bundle 'scrooloose/syntastic'
Bundle 'tomtom/tlib_vim'
Bundle 'MarcWeber/vim-addon-mw-utils'
Bundle 'Lokaltog/vim-powerline'
Bundle 'garbas/vim-snipmate'
Bundle 'tpope/vim-surround'

Bundle 'vim-scripts/OmniCppComplete'
Bundle 'vim-scripts/taglist.vim'
" }

" General {
set background=dark         " Assume a dark background
filetype plugin indent on   " Automatically detect file types.
syntax on                   " syntax highlighting
scriptencoding utf-8

set laststatus=2           " always show the status line
set encoding=utf-8

"set autowrite                   " automatically write a file when leaving a modified buffer
set shortmess+=filmnrxoOtT      " abbrev. of messages (avoids 'hit enter')
set viewoptions=folds,options,cursor,unix,slash " better unix / windows compatibility
set history=1000                " store a ton of history (default is 20)
"set spell                       " spell checking on
set hidden                      " allow buffer switching without saving

set noswapfile

if has('persistent_undo')
   set undofile                " persistent undo is nice
   set undolevels=1000         " maximum number of changes that can be undone
   set undoreload=10000        " maximum number lines to save for undo on a buffer reload
endif

" Save/load view state (cursor position, etc)
autocmd BufWinLeave *.* silent! mkview
autocmd BufWinEnter *.* silent! loadview

" }

" Vim UI {
"set t_Co=256  " Enable 256 colors to stop the CSApprox warning.

colorscheme molokai

"set showmode                    " display the current mode
set nocursorline                 " don't highlight the current line

if has('cmdline_info')
   set ruler                   " show the ruler
   set rulerformat=%30(%=\:b%n%y%m%r%w\ %l,%c%V\ %P%) " a ruler on steroids
   set showcmd                 " show partial commands in status line and
   " selected characters/lines in visual mode
endif

set backspace=indent,eol,start  " backspace for dummies
set linespace=0                 " No extra spaces between rows
set nu                          " Line numbers on
set showmatch                   " show matching brackets/parenthesis
"set incsearch                   " find as you type search
set hlsearch                    " highlight search terms
set winminheight=0              " windows can be 0 line high
"set ignorecase                  " case insensitive search
set smartcase                   " case sensitive when uc present
set wildmenu                    " show list instead of just completing
set wildmode=longest:full,full  " command <Tab> completion, list matches, then longest common part, then all.
set whichwrap=b,s,<,>,[,]       " backspace and cursor keys wrap to
set scrolljump=0                " lines to scroll when cursor leaves screen
set scrolloff=10                " minimum lines to keep above and below cursor
"set foldenable                  " auto fold code
set list
set listchars=tab:,.,trail:.,extends:#,nbsp:. " Highlight problematic whitespace

set textwidth=80
set colorcolumn=+1
hi ColorColumn ctermbg=blue

" }

" Formatting {
set nowrap                      " wrap long lines
set smartindent                 " indent at the same level of the previous line
set shiftwidth=3                " use indents of 3 spaces
set expandtab                   " tabs are spaces, not tabs
set tabstop=3                   " an indentation every three columns
set softtabstop=3               " let backspace delete indent
set matchpairs+=<:>                " match, to be used with %
set pastetoggle=<F12>           " pastetoggle (sane indentation on pastes)
"set comments=sl:/*,mb:*,elx:*/  " auto format comment blocks

" Remove trailing whitespaces and ^M chars on write.
autocmd BufWritePre <buffer> :call setline(1,map(getline(1,"$"),'substitute(v:val,"\\s\\+$","","")'))

" Go-specific.
autocmd FileType go autocmd BufWritePre <buffer> :Fmt   " Call :Fmt on write
autocmd FileType go setlocal listchars=tab:\ \ ,trail:.,extends:#,nbsp:.
autocmd FileType go setlocal noexpandtab
autocmd FileType go setlocal shiftwidth=4
autocmd FileType go setlocal tabstop=4
autocmd FileType go setlocal softtabstop=4

" }

" Key (re)Mappings {
let mapleader=','

" Easier moving in tabs and windows
map <C-H> <C-W>h
map <C-J> <C-W>j
map <C-K> <C-W>k
map <C-L> <C-W>l
map <C-X> <C-W>x
map <C-C> <C-W>w

" Wrapped lines goes down/up to next row, rather than next line in file.
nnoremap j gj
nnoremap k gk

" The following two lines conflict with moving to top and bottom of the
" screen (have to do with tabs)
map <S-H> gT
map <S-L> gt

" Visual mode Ctrl+C copies to + buffer.
set clipboard=unnamedplus
vmap <C-C> "+y

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

cmap Tabe tabe

" Yank from the cursor to the end of the line, to be consistent with C and D.
nnoremap Y y$

" Map tab to write
nnoremap <Tab> :w<CR>

" Map leader + tab to write/quit
nnoremap <leader><Tab> :x<CR>
inoremap <leader><Tab> <Esc>:x<CR>

" Clearing highlighted search
nmap <silent> <leader>/ :nohlsearch<CR>

" Colorscheme workaround - WTF.
nmap <leader>1 :colo corporation<CR>
nmap <leader>2 :colo pw<CR>

" Shortcuts
" Change Working Directory to that of the current file
cmap cwd lcd %:p:h
cmap cd. lcd %:p:h

" For when you forget to sudo.. Really Write the file.
cmap w!! w !sudo tee % >/dev/null

" Some helpers to edit mode
" http://vimcasts.org/e/14
cnoremap %% <C-R>=expand('%:h').'/'<cr>
map <leader>ew :e %%
map <leader>es :sp %%
map <leader>ev :vsp %%
map <leader>et :tabe %%

" Adjust viewports to the same size
map <leader>= <C-w>=

" Easier horizontal scrolling
map zl zL
map zh zH

" Curly brace, then get inside block.
imap <leader>{ {<CR>}<Esc>O
" }

" ,f formats in go files (necessary? I think I prefer :Fmt on write)
autocmd FileType go nmap <leader>f :Fmt<CR>

" Plugins {
" Powerline {
let g:Powerline_symbols = 'unicode'
" }

" Syntastic {
let g:syntastic_mode_map = { 'mode': 'active',
         \ 'active_filetypes': ['cpp'],
         \ 'passive_filetypes': [] }

let g:syntastic_check_on_open=1
let g:syntastic_enable_signs=1
let g:syntastic_auto_loc_list=2
let g:syntastic_loc_list_height=5    " height of location list
let g:syntastic_error_symbol='✗'
let g:syntastic_warning_symbol='⚠'
let g:syntastic_stl_format = '[%E{Err: %fe #%e}%B{, }%W{Warn: %fw #%w}]'

let g:syntastic_cpp_check_header=0   " don't check headers from cpps
let g:syntastic_cpp_compiler_options=' -g -Wall -Wextra'
" }

" taglist.vim {
map <leader>t :TlistToggle<CR><C-H>
" }
" }



" }
