" ==============================================================================
" Plugins
" ==============================================================================

call plug#begin('~/.vim/plugged')

" Language Server Protocol implementation
Plug 'autozimu/LanguageClient-neovim', { 'branch': 'next', 'do': 'bash install.sh', 'for': 'haskell' }

" Improved jumping around with f, t, F, T
Plug 'easymotion/vim-easymotion'

" Plug 'jceb/vim-orgmode'

" Automatically insert/delete parens, braces, quotes, etc
Plug 'jiangmiao/auto-pairs'

" Fuzzy search source code, files, etc
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

Plug 'junegunn/vim-easy-align'

" Highlight yanks
Plug 'machakann/vim-highlightedyank'

" Vim quickfix improvements
Plug 'romainl/vim-qf'

" File browser, only load when used
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }

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

" autocmd FileType haskell set signcolumn=yes

" ==============================================================================
" Key mappings
" ==============================================================================

" ------------------------------------------------------------------------------
" All modes
" ------------------------------------------------------------------------------

" I don't use ;, but I use :, so make : easier to type
" FIXME What does ; do anyhow?
map ; :

" Disable useless 'ex' mode and annoying command search q: that I never use
map Q <Nop>
map q: <Nop>

" [vim-easymotion]
" Use easymotion to replace f, t behavior. F, T are like supercharged f, t in
" that they can search for any number of characters at a time
map f <Plug>(easymotion-bd-f)
map F <Plug>(easymotion-bd-fn)
map t <Plug>(easymotion-bd-t)
map T <Plug>(easymotion-bd-tn)

" ------------------------------------------------------------------------------
" Normal mode
" ------------------------------------------------------------------------------

" Prevent the cursor from jumping past a wrapped line when moving up and down
nmap j gj
nmap k gk

" Make Y yank to the end of line, similar to how C and D behave
nnoremap Y y$

" Map Ctrl-T to new tab, just like in Chrome
nnoremap <silent> <C-t> :tabnew<Enter>

" Center the screen after jumping to the next highlight
nnoremap n nzz
nnoremap N Nzz
nnoremap * *zz

" Move tabs with Shift-hl
nnoremap <S-h> gT
nnoremap <S-l> gt

" Move windows with Ctrl+hjkl
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

" <Enter> to clear the current search
nnoremap <silent> <Enter> :nohlsearch<Enter>

" <Tab> to save
nnoremap <silent> <Tab> :w<Enter>

" [surround]
nmap ds <Plug>Dsurround
nmap cs <Plug>Csurround w
nmap cS <Plug>Csurround W

" [exchange]
nmap cx <Plug>(Exchange)
nmap cX <Plug>(ExchangeLine)

" [qf]
" Toggle the quickfix ("location") menu; move thru quickfix items with Alt+jk
nmap <Space>l <Plug>(qf_qf_toggle)
nmap <A-j> <Plug>(qf_qf_next)
nmap <A-k> <Plug>(qf_qf_prev)

" [commentary]
" Toggle comment (map to default vim-commentary bindings)
nmap <Space>m <Plug>CommentaryLine

" [fzf]
" Fuzzy file search, both git- and everything-variants
nnoremap <Space>o :GFiles<Enter>
nnoremap <Space>O :Files<Enter>

" [fzf]
" Space-f ("find") the word under the cursor
nnoremap <Space>f :Ag <C-r><C-w><Enter>

" [LanguageClient]
nnoremap <Space>sc :call LanguageClient_textDocument_references()<Enter>
autocmd FileType haskell nnoremap <Space>p :call LanguageClient_textDocument_formatting()<Enter>
nnoremap <Space>ss :call LanguageClient_textDocument_documentSymbol()<Enter>
nnoremap <F2> :call LanguageClient_textDocument_rename()<Enter>
nnoremap K :call LanguageClient_textDocument_hover()<Enter>
nnoremap gd :call LanguageClient_textDocument_definition()<Enter>
nnoremap <silent> <Space>sm :call LanguageClient_contextMenu()<Enter>

" [elm-vim]
" Space-p ("pretty ") to format Elm code
autocmd FileType elm nnoremap <buffer> <silent> <Space>p :ElmFormat<Enter>

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

" ------------------------------------------------------------------------------
" quickfix
" ------------------------------------------------------------------------------

" On <Enter>, go to error and close quickfix list
autocmd BufReadPost quickfix nnoremap <silent> <buffer> <Enter> <Enter>:cclose<Enter>

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

" ==============================================================================
" Autocommands
" ==============================================================================

" Start ghcid automatically
" autocmd FileType haskell autocmd BufWinEnter * :call <SID>StartGhcid()

" Jump to last cursor position on file open
autocmd BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$") | exe "normal! g`\"" | endif

" Strip trailing whitespace on save
autocmd BufWritePre * :call <SID>StripTrailingWhitespaces()

" ==============================================================================
" Abbreviations
" ==============================================================================

autocmd FileType haskell iabbrev zalpha α
autocmd FileType haskell iabbrev zbeta β
autocmd FileType haskell iabbrev zchi χ
autocmd FileType haskell iabbrev zdelta δ
autocmd FileType haskell iabbrev zepsilon ε
autocmd FileType haskell iabbrev zeta η
autocmd FileType haskell iabbrev zgamma γ
autocmd FileType haskell iabbrev ziota ι
autocmd FileType haskell iabbrev zkappa κ
autocmd FileType haskell iabbrev zlambda λ
autocmd FileType haskell iabbrev zmu μ
autocmd FileType haskell iabbrev znu ν
autocmd FileType haskell iabbrev zomega ω
autocmd FileType haskell iabbrev zphi φ
autocmd FileType haskell iabbrev zpi π
autocmd FileType haskell iabbrev zpsi ψ
autocmd FileType haskell iabbrev zrho ρ
autocmd FileType haskell iabbrev zsigma σ
autocmd FileType haskell iabbrev ztau τ
autocmd FileType haskell iabbrev ztheta θ
autocmd FileType haskell iabbrev zupsilon υ
autocmd FileType haskell iabbrev zxi ξ
autocmd FileType haskell iabbrev zzeta ζ

autocmd FileType haskell iabbrev zDelta Δ
autocmd FileType haskell iabbrev zGamma Γ
autocmd FileType haskell iabbrev zLambda Λ
autocmd FileType haskell iabbrev zOmega Ω
autocmd FileType haskell iabbrev zPhi Φ
autocmd FileType haskell iabbrev zPi Π
autocmd FileType haskell iabbrev zPsi Ψ
autocmd FileType haskell iabbrev zSigma Σ
autocmd FileType haskell iabbrev zTheta Θ
autocmd FileType haskell iabbrev zXi Ξ

autocmd FileType haskell iabbrev zforall ∀
autocmd FileType haskell iabbrev zexists ∃
autocmd FileType haskell iabbrev zbottom ⊥

autocmd FileType haskell iabbrev zA 𝔸
autocmd FileType haskell iabbrev zB 𝔹
autocmd FileType haskell iabbrev zC ℂ
autocmd FileType haskell iabbrev zD 𝔻
autocmd FileType haskell iabbrev zE 𝔼
autocmd FileType haskell iabbrev zF 𝔽
autocmd FileType haskell iabbrev zG 𝔾
autocmd FileType haskell iabbrev zH ℍ
autocmd FileType haskell iabbrev zI 𝕀
autocmd FileType haskell iabbrev zJ 𝕁
autocmd FileType haskell iabbrev zK 𝕂
autocmd FileType haskell iabbrev zL 𝕃
autocmd FileType haskell iabbrev zM 𝕄
autocmd FileType haskell iabbrev zN ℕ
autocmd FileType haskell iabbrev zO 𝕆
autocmd FileType haskell iabbrev zP ℙ
autocmd FileType haskell iabbrev zQ ℚ
autocmd FileType haskell iabbrev zR ℝ
autocmd FileType haskell iabbrev zS 𝕊
autocmd FileType haskell iabbrev zT 𝕋
autocmd FileType haskell iabbrev zU 𝕌
autocmd FileType haskell iabbrev zV 𝕍
autocmd FileType haskell iabbrev zW 𝕎
autocmd FileType haskell iabbrev zX 𝕏
autocmd FileType haskell iabbrev zY 𝕐
autocmd FileType haskell iabbrev zZ ℤ
autocmd FileType haskell iabbrev zzgamma ℽ
autocmd FileType haskell iabbrev zzGamma ℾ
autocmd FileType haskell iabbrev zzpi ℼ
autocmd FileType haskell iabbrev zzPi ℿ

autocmd FileType haskell iabbrev zeq ≡
autocmd FileType haskell iabbrev zne ≠
autocmd FileType haskell iabbrev zle ≤
autocmd FileType haskell iabbrev zge ≥
autocmd FileType haskell iabbrev zdot ∘
autocmd FileType haskell iabbrev znot ¬
autocmd FileType haskell iabbrev zand ∧
autocmd FileType haskell iabbrev zor ∨
autocmd FileType haskell iabbrev zempty ∅
autocmd FileType haskell iabbrev zunion ∪
autocmd FileType haskell iabbrev zintersect ∩

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

let g:exchange_no_mappings = 1

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

" Specify the language-specific executables to run the LSP server
let g:LanguageClient_serverCommands = {} " { 'haskell': ['hie-wrapper', '--lsp', '-d', '-l', '.HieWrapperLog'] }

" Use global settings.json file
let g:LanguageClient_settingsPath = "/home/mitchell/.config/lsp/settings.json"

" LanguageClient doesn't seem to work very well, so verbosely log everything it
" tries to do.
let g:LanguageClient_loggingLevel = 'DEBUG'
let g:LanguageClient_loggingFile = ".LanguageClientLog"

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

" Notes to myself:
"
" 'o' in visual swaps cursor location
" g<C-a> in visual mode turns 1111 into 1234
" gi
" gv
