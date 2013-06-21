set nocompatible

if has('autocmd')
   filetype on
end

set rtp+=~/.vim/plugin
set rtp+=~/.vim/bundle/vundle

set rtp+=$GOROOT/misc/vim
call vundle#rc()

Bundle 'gmarik/vundle'

Bundle 'vim-scripts/a.vim'
Bundle   'godlygeek/csapprox'
Bundle        'kien/ctrlp.vim'
Bundle     'ujihisa/neco-ghc'
Bundle      'Shougo/neocomplcache'
Bundle  'scrooloose/nerdcommenter'
Bundle 'vim-scripts/OmniCppComplete'
Bundle    'ervandew/supertab'
Bundle  'scrooloose/syntastic'
Bundle   'godlygeek/tabular'
Bundle  'majutsushi/tagbar'
Bundle      'tomtom/tlib_vim'
Bundle         'dag/vim2hs'
Bundle   'MarcWeber/vim-addon-mw-utils'
Bundle        'bitc/vim-hdevtools'
Bundle    'Lokaltog/vim-powerline'
Bundle      'garbas/vim-snipmate'
Bundle       'tpope/vim-fugitive'
Bundle       'tpope/vim-surround'
Bundle 'jcf/vim-latex'

"Bundle    'Twinside/vim-haskellConceal'
"Bundle 'lukerandall/haskellmode-vim'
"Bundle 'FredKSchott/CoVim'
"Bundle 'Shougo/vimproc'
"Bundle 'eagletmt/ghcmod-vim'

set background=dark         " Assume a dark background

" Enable loading the plugin files for specific file types.
filetype plugin on

" Enable loading the indent file for specific file types.
filetype indent on
syntax on                   " syntax highlighting
scriptencoding utf-8

" When a file has been detected to have been changed of Vim and it has not been
" changed inside Vim, automatically read it again.
set autoread

" Write the contents of the file, if it has been modified, on each :next,
" :rewind, :last, :first, :previous, :stop, :suspend, :tag, :!, :make, CTRL-]
" and CTRL-^ command; and when a :buffer, CTRL-O, CTRL-I, '{A-Z0-9}, or
" `{A-Z0-9} command takes one to another file.
set autowrite

" Set the character encoding used inside Vim.
set encoding=utf-8

set foldmethod=syntax

" The last window will always have a status line.
set laststatus=2

" Helps to avoid all the hit-enter prompts caused by file messages.
set shortmess=aoOW

" Show (partial) command in the last line of the screen.
set showcmd

" When off a buffer is unloaded when it is abandoned.  When on a buffer becomes
" hidden when it is abandoned.
set hidden

" Do not use a swapfile for the buffer.
set noswapfile

" Do not make a backup before overwriting a file.
set nowritebackup

" Maximum number of changes that can be undone.
set undolevels=1000         " maximum number of changes that can be undone

" Save the whole buffer for undo when releoading it. Only happens when this
" option is negative or when the number of lines is smaller than the value of
" this option.
set undoreload=-1

if has('persistent_undo')
   " List of directory names for undo files. NOTE: Directory must already exist.
   set undodir=~/.vim/backups

   " Automatically save undo history to an undo file when writing a buffer to a
   " file, and restores undo history from the same file on buffer read.
   set undofile
endif


" Save/load view state (cursor position, etc)
au BufWinLeave *.* silent! mkview
au BufWinEnter *.* silent! loadview

colorscheme molokai

set backspace=indent,eol,start  " backspace for dummies
set linespace=0                 " No extra spaces between rows
set number                      " Line numbers on
set relativenumber              " Relative line numbers
set foldenable
set cursorline
set showmatch                   " show matching brackets/parenthesis
set matchpairs+=<:>             " match < >
set hlsearch                    " highlight search terms
set winminheight=0              " windows can be 0 line high
set winminwidth=0
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

au FileType python setlocal ts=4 sw=4 sts=4

" Key Mappings
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

" Better mnemonics for fold open-all (zO) and fold close-all (zC)
map zO zR
map zC zM

" a.vim
map <leader>av :AV<CR>
map <leader>a<Space> :A<CR>

" Latex
let g:tex_flavor='latex'

" NeoComplCache
let g:neocomplcache_enable_at_startup = 1

" Powerline
let g:Powerline_symbols = 'unicode'

" Syntastic
let g:syntastic_mode_map = {
      \ 'mode': 'active',
      \ 'passive_filetypes': ['java']
      \ }
let g:syntastic_always_populate_loc_list=1 " Always stick any detected errors into the loclist.
let g:syntastic_check_on_open=1            " Do syntax checks when buffers are first loaded.
let g:syntastic_cpp_check_header=0   " don't check headers from cpps
let g:syntastic_cpp_compiler_options=' -g -Wall -Wextra'
let g:syntastic_error_symbol='✗'
let g:syntastic_haskell_checkers=['hdevtools', 'ghc_mod']
let g:syntastic_loc_list_height=5 " Height of the location list.
let g:syntastic_stl_format = '[%E{Err: %fe #%e}%B{, }%W{Warn: %fw #%w}]'
let g:syntastic_warning_symbol='⚠'

" Tabular
"" TODO haskell-only
map <leader>hb :Tabular haskell_bindings<CR>
map <leader>hd :Tabular haskell_do_arrows<CR>
map <leader>hi :Tabular haskell_imports<CR>
map <leader>hp :Tabular haskell_pattern_arrows<CR>
map <leader>ht :Tabular haskell_types<CR>

" Tagbar
map <leader>t :TagbarToggle<CR>

" Hdevtools
au FileType haskell nnoremap <buffer> <F1> :HdevtoolsType<CR>
au FileType haskell nnoremap <buffer> <F2> :HdevtoolsInfo<CR>
au FileType haskell nnoremap <buffer> <F3> :HdevtoolsClear<CR>
