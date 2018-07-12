" ==============================================================================
" Plugins
" ==============================================================================

call plug#begin('~/.vim/plugged')

" Language Server Protocol implementation
" Plug 'autozimu/LanguageClient-neovim', { 'branch': 'next', 'do': 'bash install.sh', 'for': 'haskell' }

" Improved jumping around with f, t, F, T
Plug 'easymotion/vim-easymotion'

" Plug 'jceb/vim-orgmode'

" Automatically insert/delete parens, braces, quotes, etc
Plug 'jiangmiao/auto-pairs'

" Fuzzy search source code, files, etc
" :help fzf-vim
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

Plug 'junegunn/vim-easy-align'

" Highlight yanks
Plug 'machakann/vim-highlightedyank'

" Vim quickfix improvements
Plug 'romainl/vim-qf'

" File browser, only load when used
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }

Plug 'terryma/vim-multiple-cursors'

Plug 'tommcdo/vim-exchange'

" Improved 'ga' behavior (shows unicode code point, vim digraph, etc. of
" character under cursor)
Plug 'tpope/vim-characterize'

" Quick (un-)commenting
Plug 'tpope/vim-commentary'

" Make '.' repeat more things out of the box
Plug 'tpope/vim-repeat'

" Some surround helpers.
" :help surround
Plug 'tpope/vim-surround'

" Lightweight status line
" TODO: Replace this with a custom statusline
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" Language-specific syntax highlighting and such
Plug 'ElmCast/elm-vim', { 'for': 'elm' }
Plug 'LnL7/vim-nix', { 'for': 'nix' }
Plug 'neovimhaskell/haskell-vim'
Plug 'purescript-contrib/purescript-vim', { 'for': 'purescript' }

Plug 'ndmitchell/ghcid', { 'rtp': 'plugins/nvim', 'for': 'haskell' }

Plug 'chriskempson/base16-vim'
Plug 'morhetz/gruvbox' " Need this for the airline theme :)

Plug 'SirVer/ultisnips'

call plug#end()
" Automatically calls syntax on, filetype plugin indent on

" ==============================================================================
" Basic settings
" ==============================================================================

" Colorscheme requires base15-shell, which writes ~/.vimrc_background
" FIXME Would be nice to auto-update airline theme too, somehow
if filereadable(expand("~/.vimrc_background"))
  let base16colorspace=256
  source ~/.vimrc_background
endif

set background=dark
set autowrite                     " write when leaving a buffer
set clipboard=unnamed,unnamedplus " yank also copies to both clipboards
set colorcolumn=81                " highlight column 81
set cursorline                    " higlight the current line
set expandtab                     " convert tabs to spaces
set hidden                        " don't abandon out-of-sight buffers
set ignorecase
set inccommand=split              " show live command substitutions
set incsearch                     " search while typing
set lazyredraw                    " don't draw during e.g. applying a macro
set linebreak                     " wrap lines in a more visually pleasing way
set list                          " show trailing whitespace, tabs, etc.
set nofoldenable                  " never fold
set nojoinspaces
set scrolloff=10                  " leave lines when scrolling
set shiftround                    " shift to multiple of shiftwidth
set shiftwidth=2
set smartcase
set smartindent
set softtabstop=2
set undofile                      " persist undo history across buffer exits
set wildmenu                      " complete commands with a little menu
set wildmode=list:longest,full

" ==============================================================================
" Key mappings
" ==============================================================================

" ------------------------------------------------------------------------------
" All modes
" ------------------------------------------------------------------------------

" I don't use ;, but I use :, so make : easier to type
noremap ; :

" Disable annoying command search 'q:' that I never use
map q: <Nop>

" [vim-easymotion]
" gf to 'go find' some far-away character (above or below)
map gf <Plug>(easymotion-bd-f)

" ------------------------------------------------------------------------------
" Normal mode
" ------------------------------------------------------------------------------

" Prevent the cursor from jumping past a wrapped line when moving up and down
nmap j gj
nmap k gk

" Make Y yank to the end of line, similar to how C and D behave
nnoremap Y y$

" <Enter> to clear the current search
nnoremap <silent> <Enter> :nohlsearch<Enter>

" Record macros with Q instead of q
nnoremap Q q
nnoremap q <Nop>

" <Tab> to save
nnoremap <silent> <Tab> :w<Enter>

" Map Ctrl-T to new tab, just like in Chrome
" nnoremap <silent> <C-t> :tabnew<Enter>

" Don't highlight matches *and* jump at the same time; only highlight
nnoremap * *``
nnoremap # #``

" Center the screen after jumping to the next highlight
nnoremap n nzz
nnoremap N Nzz

" Make K pull the current line up, similar to how J pulls the following line up
nnoremap K kJ

" Move tabs with Shift-hl
" Trying to wean myself off tabs, so I commented this out
" nnoremap <S-h> gT
" nnoremap <S-l> gt

" Move buffers with Ctrl+jk
nnoremap <silent> <C-j> :bn<Enter>
nnoremap <silent> <C-k> :bp<Enter>

" Move vertical splits with Ctrl+hl (sorry, horizontal splits)
nnoremap <C-h> <C-w>h
nnoremap <C-l> <C-w>l

" [repeat]
" q-hjkl> to push code around. repeat.vim is used to make these commands
" repeatable with '.'
" FIXME Would be nice to not clobber the 'z' mark
nmap qh <Plug>MyNmapQh
nmap qj <Plug>MyNmapQj
nmap qk <Plug>MyNmapQk
nmap ql i<Space><Esc>
nmap q> <Plug>MyNmapQgt
nmap q< <Plug>MyNmapQlt
nnoremap <silent> <Plug>MyNmapQh :call <SID>QH()<Enter>:call repeat#set("\<Plug>MyNmapQh")<Enter>
nnoremap <silent> <Plug>MyNmapQj mzo<Esc>`z:call repeat#set("\<Plug>MyNmapQj")<Enter>
nnoremap <silent> <Plug>MyNmapQk mzO<Esc>`z:call repeat#set("\<Plug>MyNmapQk")<Enter>
nnoremap <silent> <Plug>MyNmapQgt mzi<Tab><Esc>`z:call repeat#set("\<Plug>MyNmapQgt")<Enter>
nnoremap <silent> <Plug>MyNmapQlt :call <SID>Qlt()<Enter>:call repeat#set("\<Plug>MyNmapQlt")<Enter>

" [surround]
" ds to delete surround
" cs to surround inner word
" cS to surround inner WORD
nmap ds <Plug>Dsurround
nmap cs <Plug>Csurround w
nmap cS <Plug>Csurround W

" [exchange]
" X ("exchange") once to yank, X again to exchange with the first yank
" Manually make [exhange] replace 'w' with 'e', as vim does for e.g. 'c'
"
" XX to exchange-yank the whole line
nmap Xw <Plug>(Exchange)e
nmap XW <Plug>(Exchange)E
nmap X <Plug>(Exchange)
nmap XX <Plug>(ExchangeLine)

" foo-bar foo-baz

" [qf]
" Toggle the quickfix ("location") menu; move thru quickfix items with Alt+jk
nmap <Space>l <Plug>(qf_qf_toggle)
nmap <A-j> <Plug>(qf_qf_next)
nmap <A-k> <Plug>(qf_qf_prev)

" [commentary]
" Toggle comment
nmap <Space>m <Plug>CommentaryLine

" [fzf]
" Space-o ("open") to fuzzy file search, both git- and everything-variants
nnoremap <Space>o :GFiles<Enter>
nnoremap <Space>O :Files<Enter>
" Space-f ("find") the word under the cursor
nnoremap <Space>f :Ag <C-r><C-w><Enter>
" Space-k (because it's a home-row key) to fuzzy-search buffers
nnoremap <Space>k :Buffers<Enter>

" [LanguageClient]
" nnoremap <Space>sc :call LanguageClient_textDocument_references()<Enter>
" nnoremap <Space>ss :call LanguageClient_textDocument_documentSymbol()<Enter>
" nnoremap <F2> :call LanguageClient_textDocument_rename()<Enter>
" nnoremap gt :call LanguageClient_textDocument_hover()<Enter>
" nnoremap gd :call LanguageClient_textDocument_definition()<Enter>
" nnoremap <silent> <Space>sm :call LanguageClient_contextMenu()<Enter>

" ------------------------------------------------------------------------------
" Insert mode
" ------------------------------------------------------------------------------

" Ctrl+space for omnicomplete
imap <C-Space> <C-x><C-o>

" When a popup menu is visible, move thru it with tab and select with enter
inoremap <expr> <Tab>   pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <Enter> pumvisible() ? "\<C-y>" : "\<Enter>"

" ------------------------------------------------------------------------------
" Visual mode
" ------------------------------------------------------------------------------

" Make visual mode * work like normal mode *
vnoremap * y/<C-r>"<Enter>zz

" [exchange]
xmap X <Plug>(Exchange)

" [vim-easy-align]
" Enter to align things with
vmap <Enter> <Plug>(LiveEasyAlign)

" [vim-commentary]
" Toggle comment
vmap <Space>m <Plug>Commentary

" [vim-surround]
xmap S <Plug>VSurround

" [fzf.vim]
" Space-f ("find") the selected contents
vmap <Space>f "0y:Ag <C-r>0<Enter>

" ------------------------------------------------------------------------------
" Terminal mode
" ------------------------------------------------------------------------------

" Escape terminal mode with <Esc>
tnoremap <Esc> <C-\><C-n>
tnoremap <A-[> <Esc>

" ==============================================================================
" Functions
" ==============================================================================

" Remove trailing whitespace, then restore cursor position
function! <SID>StripTrailingWhitespaces()
  let l = line(".")
  let c = col(".")
  %s/\s\+$//e
  call cursor(l, c)
endfun

" [ghcid]
" If a .ghcid file exists, start ghcid
function! <SID>StartGhcid()
  if filereadable(".ghcid")
    exec "Ghcid"
  elseif filereadable("cabal.project")
    exec "Ghcid -c 'cabal new-repl'"
  elseif filereadable("stack.yaml")
    exec "Ghcid -c 'stack ghci'"
  elseif filereadable(expand('%'))
    exec "Ghcid -c 'ghci " . expand('%') . "'"
  endif
endfun

" qh behavior: move code to the left if there's room
function! <SID>QH()
  if getline('.')[0] == ' '
    exec "norm hmz0x`zi\<Space>\<Esc>l"
  endif
endfunction

" q< behavior: shift code to the left if there's room
" FIXME: This just assumes shiftwidth=2, and ignore shiftround
function! <SID>Qlt()
  let l = getline('.')
  if l[0] == ' ' && l[1] == ' '
    exec "norm 2hmz02x`zi\<Space>\<Space>\<Esc>l"
  endif
endfunction

" ==============================================================================
" Autocommands
" ==============================================================================

" Jump to last cursor position on file open
autocmd BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$") | exe "normal! g`\"" | endif

" Strip trailing whitespace on save
autocmd BufWritePre * :call <SID>StripTrailingWhitespaces()

" ==============================================================================
" Abbreviations
" ==============================================================================

iabbrev zalpha α
iabbrev zbeta β
iabbrev zchi χ
iabbrev zdelta δ
iabbrev zepsilon ε
iabbrev zeta η
iabbrev zgamma γ
iabbrev ziota ι
iabbrev zkappa κ
iabbrev zlambda λ
iabbrev zmu μ
iabbrev znu ν
iabbrev zomega ω
iabbrev zphi φ
iabbrev zpi π
iabbrev zpsi ψ
iabbrev zrho ρ
iabbrev zsigma σ
iabbrev ztau τ
iabbrev ztheta θ
iabbrev zupsilon υ
iabbrev zxi ξ
iabbrev zzeta ζ

iabbrev zDelta Δ
iabbrev zGamma Γ
iabbrev zLambda Λ
iabbrev zOmega Ω
iabbrev zPhi Φ
iabbrev zPi Π
iabbrev zPsi Ψ
iabbrev zSigma Σ
iabbrev zTheta Θ
iabbrev zXi Ξ

iabbrev zforall ∀
iabbrev zexists ∃
iabbrev zbottom ⊥

iabbrev zA 𝔸
iabbrev zB 𝔹
iabbrev zC ℂ
iabbrev zD 𝔻
iabbrev zE 𝔼
iabbrev zF 𝔽
iabbrev zG 𝔾
iabbrev zH ℍ
iabbrev zI 𝕀
iabbrev zJ 𝕁
iabbrev zK 𝕂
iabbrev zL 𝕃
iabbrev zM 𝕄
iabbrev zN ℕ
iabbrev zO 𝕆
iabbrev zP ℙ
iabbrev zQ ℚ
iabbrev zR ℝ
iabbrev zS 𝕊
iabbrev zT 𝕋
iabbrev zU 𝕌
iabbrev zV 𝕍
iabbrev zW 𝕎
iabbrev zX 𝕏
iabbrev zY 𝕐
iabbrev zZ ℤ
iabbrev zzgamma ℽ
iabbrev zzGamma ℾ
iabbrev zzpi ℼ
iabbrev zzPi ℿ

iabbrev zeq ≡
iabbrev zne ≠
iabbrev zle ≤
iabbrev zge ≥
iabbrev zdot ∘
iabbrev znot ¬
iabbrev zand ∧
iabbrev zor ∨
iabbrev zempty ∅
iabbrev zunion ∪
iabbrev zintersect ∩

" ------------------------------------------------------------------------------
" Command mode
" ------------------------------------------------------------------------------

" [UltiSnips]
" Edit snippets of current file type
cabbrev snipedit UltiSnipsEdit

" ==============================================================================
" Plugin settings
" ==============================================================================

" ------------------------------------------------------------------------------
" airline
" ------------------------------------------------------------------------------

let g:airline_symbols_ascii = 1
let g:airline_theme='gruvbox'

" ------------------------------------------------------------------------------
" AutoPairs
" ------------------------------------------------------------------------------

" Which symbols are automatically paired
let g:AutoPairs = { '(': ')', '[': ']', '{': '}', "'": "'", '"': '"', '`': '`' }

" Let AutoPairs handle <Backspace> (deletes pair)
let g:AutoPairsMapBS = 1

" Let AutoPairs insert a space before the closing pair
let g:AutoPairsMapSpace = 1

" Don't handle <Enter> in insert mode specially (cause it's brokenish)
let g:AutoPairsMapCR = 0
let g:AutoPairsCenterLine = 0

" Disable a bunch of random key bindings
let g:AutoPairsShortcutToggle = ''
let g:AutoPairsShortcutFastWrap = ''
let g:AutoPairsShortcutJump = ''
let g:AutoPairsShortcutBackInsert = ''
let g:AutoPairsMapCh = 0


" ------------------------------------------------------------------------------
" EasyMotion
" ------------------------------------------------------------------------------

" Don't let EasyMotion write any default mappings
let g:EasyMotion_do_mapping = 0

let g:EasyMotion_smartcase = 1
let g:EasyMotion_use_upper = 1
let g:EasyMotion_keys = 'ASDGHKLQWERTYUIOPZXCVBNMFJ;'

" ------------------------------------------------------------------------------
" Elm
" ------------------------------------------------------------------------------

" Don't run elm-format on save
let g:elm_format_autosave = 0

" ------------------------------------------------------------------------------
" exchange
" ------------------------------------------------------------------------------

" Don't make any default key mappings
let g:exchange_no_mappings = 1

" ------------------------------------------------------------------------------
" fzf
" ------------------------------------------------------------------------------

" If the buffer is already open in another tab or window, jump to it rather
" than replace the current buffer (which would open 2 copies)
let g:fzf_buffers_jump = 1

" ------------------------------------------------------------------------------
" ghcid
" ------------------------------------------------------------------------------

let g:ghcid_background = 1

" ------------------------------------------------------------------------------
" Haskell
" ------------------------------------------------------------------------------

" let g:haskell_indent_disable = 1
let g:haskell_enable_backpack = 1
let g:haskell_enable_pattern_synonyms = 1
let g:haskell_enable_quantification = 1
" let g:haskell_indent_before_where = 2
let g:haskell_indent_case = 2
let g:haskell_indent_if = 2
let g:haskell_indent_in = 0
let g:haskell_indent_let = 2
" let g:haskell_indent_where = 2
" let g:haskell_indent_bare_where = 2
" let g:haskell_classic_highlighting = 1

" ------------------------------------------------------------------------------
" highlightedyank
" ------------------------------------------------------------------------------

" Highlight yank for 500ms
let g:highlightedyank_highlight_duration = 500
let g:highlightedyank_max_lines = 50

" ------------------------------------------------------------------------------
" LanguageClient
" ------------------------------------------------------------------------------

" " Specify the language-specific executables to run the LSP server
" let g:LanguageClient_serverCommands = {} " { 'haskell': ['hie-wrapper', '--lsp', '-d', '-l', '.HieWrapperLog'] }

" " Use global settings.json file
" let g:LanguageClient_settingsPath = "/home/mitchell/.config/lsp/settings.json"

" " LanguageClient doesn't seem to work very well, so verbosely log everything it
" " tries to do.
" let g:LanguageClient_loggingLevel = 'DEBUG'
" let g:LanguageClient_loggingFile = ".LanguageClientLog"

" ------------------------------------------------------------------------------
" multiple-cursors
" ------------------------------------------------------------------------------

let g:multi_cursor_use_default_mapping = 0
let g:multi_cursor_start_word_key      = '<C-n>'
" let g:multi_cursor_select_all_word_key = '<A-n>'
" let g:multi_cursor_start_key           = 'g<C-n>'
" let g:multi_cursor_select_all_key      = 'g<A-n>'
let g:multi_cursor_next_key            = '<C-n>'
let g:multi_cursor_prev_key            = '<C-p>'
" let g:multi_cursor_skip_key            = '<C-x>'
let g:multi_cursor_quit_key            = '<Esc>'

" ------------------------------------------------------------------------------
" NERDTree
" ------------------------------------------------------------------------------

nnoremap <silent> <Space>n :NERDTreeToggle<Enter>

" ------------------------------------------------------------------------------
" surround
" ------------------------------------------------------------------------------

" Don't let surround provide any magic mappings
let g:surround_no_mappings = 1

" ------------------------------------------------------------------------------
" UltiSnips
" ------------------------------------------------------------------------------

" Tell UltiSnips to use Python 3 (in case auto-detect doesn't work)
let g:UltiSnipsUsePythonVersion = 3

" Read snippets from this directory
let g:UltiSnipsSnippetsDir = "~/.config/nvim/UltiSnips"

" Open snippets file with a horizontal split with :snipedit
let g:UltiSnipsEditSplit = 'horizontal'

" Unset annoying key mappings that can't be avoided
let g:UltiSnipsExpandTrigger="<C-j>"
let g:UltiSnipsListSnippets="<Nop>"
let g:UltiSnipsJumpForwardTrigger="<C-j>"
let g:UltiSnipsJumpBackwardTrigger="<C-k>"

" ==============================================================================
" nvim-gtk settings
" ==============================================================================

if exists('g:GtkGuiLoaded')
  call rpcnotify(1, 'Gui', 'Font', 'Hasklig 14')
endif

" ==============================================================================
" Filetype-specific settings
" ==============================================================================

" ------------------------------------------------------------------------------
" Elm
" ------------------------------------------------------------------------------

" [elm-vim]
" Space-p ("pretty ") to format Elm code
autocmd FileType elm nnoremap <buffer> <silent> <Space>p :ElmFormat<Enter>

" ------------------------------------------------------------------------------
" fzf
" ------------------------------------------------------------------------------

autocmd! FileType fzf
autocmd FileType fzf setlocal laststatus=0
  \| autocmd BufLeave <buffer> setlocal laststatus=2
autocmd FileType fzf nnoremap <silent> <buffer> <Esc> :q<Enter>

" ------------------------------------------------------------------------------
" Haskell
" ------------------------------------------------------------------------------

autocmd FileType haskell call <SID>IsFiletypeHaskell()
function <SID>IsFiletypeHaskell()

  " Always \ as a lambda
  " syn match Lambda /\\/ conceal cchar=λ
  " set conceallevel=2
  " set concealcursor=nvic
  " set signcolumn=yes

  " syn match Arrow /->$/ conceal cchar=a


  nnoremap <Space>p :call LanguageClient_textDocument_formatting()<Enter>

  " Start ghcid automatically
  " autocmd BufWinEnter * :call <SID>StartGhcid()

endfunction

" ------------------------------------------------------------------------------
" quickfix
" ------------------------------------------------------------------------------

" On <Enter>, go to error and close quickfix list
autocmd FileType qf nnoremap <silent> <buffer> <Enter> <Enter>:cclose<Enter>

" ==============================================================================

" Notes to myself:
"
" 'o' in visual swaps cursor location
" g<C-a> in visual mode turns 1111 into 1234
" gi
" gv
