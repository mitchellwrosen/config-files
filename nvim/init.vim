" Big default verb/action changes:
"
" * J = page down
" * K = page up
" * Q = record (to repurpose q as a more common verb than macro)
" * q = push text around (don't love this one)
" * s = surround word
" * S = surround WORD
" * SS = surround line
" * X = exchange
"
" TODO
" * Replace vim-airline with custom status line
" * Open ghcid buffer on hovering over something in quickfix
" * shada?
" * Fix undo behavior for composite actions like K
" * Only use AutoPairs if not up against a character
" * Alt-K doesn't seem to work
" * Make 'sp' repeatable
" * Write function to make ds search for nearest surround and delete it
" * U to redo
" * vim-signify
" * better function snippet (add args as I add types to type sig)

" ==============================================================================
" Plugins
" ==============================================================================

cal plug#begin('~/.vim/plugged')

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
" Plug 'vim-airline/vim-airline'
" Plug 'vim-airline/vim-airline-themes'

" Language-specific syntax highlighting and such
Plug 'ElmCast/elm-vim', { 'for': 'elm' }
Plug 'LnL7/vim-nix', { 'for': 'nix' }
Plug 'neovimhaskell/haskell-vim'
Plug 'purescript-contrib/purescript-vim', { 'for': 'purescript' }

Plug 'ndmitchell/ghcid', { 'rtp': 'plugins/nvim', 'for': 'haskell' }

Plug 'chriskempson/base16-vim'
" Plug 'morhetz/gruvbox' " Need this for the airline theme :)

Plug 'SirVer/ultisnips'

cal plug#end()
" Automatically calls syntax on, filetype plugin indent on

" ==============================================================================
" Basic settings
" ==============================================================================

" Colorscheme requires base15-shell, which writes ~/.vimrc_background
" FIXME Would be nice to auto-update airline theme too, somehow
if filereadable(expand("~/.vimrc_background"))
  let base16colorspace=256
  so ~/.vimrc_background
endif

set bg=dark
set aw                     " write when leaving a buffer
set cb=unnamed,unnamedplus " yank also copies to both clipboards
set cc=81                  " highlight column 81
set cul                    " higlight the current line
set et                     " convert tabs to spaces
set gp=ag                  " use ag to grep
set hid                    " don't abandon out-of-sight buffers
set ic                     " case-insensitive searching
set icm=split              " show live command substitutions
set is                     " search while typing
set lz                     " don't draw during e.g. applying a macro
set lbr                    " wrap lines in a more visually pleasing way
set list                   " show trailing whitespace, tabs, etc.
set nofen                  " never fold
set nojs                   " insert one space after ., ?, ! chars when joining
set noml                   " disable modelines
set nosol                  " don't jump cursor to start of line when moving
set so=10                  " leave lines when scrolling
set sr                     " shift to multiple of shiftwidth
set sw=2
set scs                    " don't ignore case if search contains uppercase char
set si                     " smart autoindenting when starting a new line
set sts=2                  " tab key makes 2 spaces
set title                  " put filename in window title
set tm=500                 " only wait .5s for key sequence to complete
set udf                    " persist undo history across buffer exits
set wmnu                   " complete commands with a little menu
set wim=list:longest,full  " wild menu completion behavior

" ==============================================================================
" Key mappings
" ==============================================================================

" ------------------------------------------------------------------------------
" All modes
" ------------------------------------------------------------------------------

" I don't use ;, but I use :, so make : easier to type
no ; :

" Disable annoying command search 'q:' that I never use
map q: <Nop>

" [vim-easymotion]
" gf to 'go find' some far-away character (above or below)
map gf <Plug>(easymotion-bd-f)

" ------------------------------------------------------------------------------
" Normal mode
" ------------------------------------------------------------------------------

" <Tab> to save
nn <silent> <Tab> :w<CR>

" <Enter> to clear the current search
nn <silent> <Enter> :noh<CR>

" Prevent the cursor from jumping past a wrapped line when moving up and down
nm j gj
nm k gk

" Big jk, and wean myself off <C-du>
nn J <C-d>
nn K <C-u>
nn <C-d> <Nop>
nn <C-u> <Nop>

" Make Y yank to the end of line, similar to how C and D behave
nn Y y$

" Record macros with Q instead of q
nn Q q
nn q <Nop>

" Map Ctrl-T to new tab, just like in Chrome
" nn <silent> <C-t> :tabnew<CR>

" Don't highlight matches *and* jump at the same time; only highlight
nn * *``
nn # #``

" Follow >>/<< shifted text around with the cursor
nm >> <Plug>MyNmapLl
nm << <Plug>MyNmapHh
nn <silent> <Plug>MyNmapLl >>ll:cal repeat#set("\<Plug>MyNmapLl")<CR>
nn <silent> <Plug>MyNmapHh <<hh:cal repeat#set("\<Plug>MyNmapHh")<CR>

" ,jk to join (and reverse join)
nn ,j m`J``
nn ,k km`J``

" Ctrl+S to search-and-replace in the file
nn <C-s> :%s//g<Left><Left>

" Move tabs with Shift-hl
" Trying to wean myself off tabs, so I commented this out
" nn <S-h> gT
" nn <S-l> gt

" Move buffers with Ctrl+jk
nn <silent> <C-j> :bn<CR>
nn <silent> <C-k> :bp<CR>

" Delete the current buffer with Space-d, or quit vim if it's the only buffer
nn <expr> <silent> <Space>d len(getbufinfo({'buflisted': 1})) ==? 1 ? ":q\<CR>" : ":bw\<CR>"

" Move vertical splits with Ctrl+hl (sorry, horizontal splits)
nn <C-h> <C-w>h
nn <C-l> <C-w>l

" [repeat]
" q-hjkl> to push code around. repeat.vim is used to make these commands
" repeatable with '.'
" FIXME Would be nice to not clobber the 'z' mark
nm qh <Plug>MyNmapQh
nm qj <Plug>MyNmapQj
nm qk <Plug>MyNmapQk
nm ql i<Space><Esc>
nm q> <Plug>MyNmapQgt
nm q< <Plug>MyNmapQlt
nn <silent> <Plug>MyNmapQh :cal <SID>QH()<CR>:cal repeat#set("\<Plug>MyNmapQh")<CR>
nn <silent> <Plug>MyNmapQj m`o<Esc>``:cal repeat#set("\<Plug>MyNmapQj")<CR>
nn <silent> <Plug>MyNmapQk m`O<Esc>``:cal repeat#set("\<Plug>MyNmapQk")<CR>
nn <silent> <Plug>MyNmapQgt m`i<Tab><Esc>``:cal repeat#set("\<Plug>MyNmapQgt")<CR>
nn <silent> <Plug>MyNmapQlt :cal <SID>Qlt()<CR>:cal repeat#set("\<Plug>MyNmapQlt")<CR>

" [surround]
" ds to delete surround and restore cursor position
" s to surround inner word and restore cursor position
" S to surround inner WORD and restore cursor position
" SS to surround current line restore cursor position
nm ds' mz<Plug>Dsurround'`zh
nm ds" mz<Plug>Dsurround"`zh
nm ds( mz<Plug>Dsurround)`zh
nm ds[ mz<Plug>Dsurround]`zh
nm ds{ mz<Plug>Dsurround}`zh
nm dsp mz<Plug>Dsurround)`zh
nm s' mz<Plug>Csurround w'`zl
nm s" mz<Plug>Csurround w"`zl
nm s( mz<Plug>Csurround w)`zl
nm s[ mz<Plug>Csurround w]`zl
nm s{ mz<Plug>Csurround w}`zl
nm sp mz<Plug>Csurround w)`zl
nm S' mz<Plug>Csurround W'`zl
nm S" mz<Plug>Csurround W"`zl
nm S( mz<Plug>Csurround W)`zl
nm S[ mz<Plug>Csurround W]`zl
nm S{ mz<Plug>Csurround W}`zl
nm Sp mz<Plug>Csurround W)`zl
nm SS' mz<Plug>Yssurround'`z
nm SS" mz<Plug>Yssurround"`z
nm SS( mz<Plug>Yssurround)`z
nm SS[ mz<Plug>Yssurround]`z
nm SS{ mz<Plug>Yssurround}`z
nm SSp mz<Plug>Yssurround)`z

" [exchange]
" X ("exchange") once to yank, X again to exchange with the first yank
" Manually make [exhange] replace 'w' with 'e', as vim does for e.g. 'c'
"
" XX to exchange-yank the whole line
nm Xw <Plug>(Exchange)e
nm XW <Plug>(Exchange)E
nm X <Plug>(Exchange)
nm XX <Plug>(ExchangeLine)

" [qf]
" Toggle the quickfix ("location") menu; move thru quickfix items with Alt+jk
nm <Space>l <Plug>(qf_qf_toggle)
nm <A-j> <Plug>(qf_qf_next)
nm <A-k> <Plug>(qf_qf_prev)

" [commentary]
" Toggle comment
nm <Space>m <Plug>CommentaryLine

" [fzf]
" Space-o ("open") to fuzzy file search, both git- and everything-variants
nn <Space>o :GFiles<CR>
nn <Space>O :Files<CR>
" Space-f ("find") the word under the cursor
nn <Space>f :Ag <C-r><C-w><CR>
" Space-k (because it's a home-row key) to fuzzy-search buffers
nn <Space>k :Buffers<CR>

" [LanguageClient]
" nn <Space>sc :cal LanguageClient_textDocument_references()<CR>
" nn <Space>ss :cal LanguageClient_textDocument_documentSymbol()<CR>
" nn <F2> :cal LanguageClient_textDocument_rename()<CR>
" nn gt :cal LanguageClient_textDocument_hover()<CR>
" nn gd :cal LanguageClient_textDocument_definition()<CR>
" nn <silent> <Space>sm :cal LanguageClient_contextMenu()<CR>

" ------------------------------------------------------------------------------
" Insert mode
" ------------------------------------------------------------------------------

" Ctrl+space for omnicomplete
im <C-Space> <C-x><C-o>

" When a popup menu is visible, move thru it with tab and select with enter
ino <expr> <Tab>   pumvisible() ? "\<C-n>" : "\<Tab>"
ino <expr> <CR> pumvisible() ? "\<C-y>" : "\<CR>"

" ------------------------------------------------------------------------------
" Visual mode
" ------------------------------------------------------------------------------

" Make visual mode * work like normal mode *
vn * y/<C-r>"<CR>zz

" After yank, leave cursor at the end of the highlight
vn y y`]

" Ctrl+S to search-and-replace
xn <C-s> :s//g<Left><Left>

" [exchange]
xm X <Plug>(Exchange)

" [vim-easy-align]
" Enter to align things with
vm <CR> <Plug>(LiveEasyAlign)

" [vim-commentary]
" Toggle comment
vm <Space>m <Plug>Commentary

" [vim-surround]
xm s' <Plug>VSurround'
xm s" <Plug>VSurround"
xm s( <Plug>VSurround)
xm s[ <Plug>VSurround]
xm s{ <Plug>VSurround}
xm sp <Plug>VSurround)

" We gave vs to vim-surround, so make S work like s did (but it doesn't seem
" very useful)
xn S s

" [fzf.vim]
" Space-f ("find") the selected contents
vm <Space>f "0y:Ag <C-r>0<CR>

" ------------------------------------------------------------------------------
" Terminal mode
" ------------------------------------------------------------------------------

" Escape terminal mode with <Esc>
tno <Esc> <C-\><C-n>
tno <A-[> <Esc>

" ==============================================================================
" Commands
" ==============================================================================

" command! Vimrc e "~/.config/nvim/init.vim"

" ==============================================================================
" Functions
" ==============================================================================

" Remove trailing whitespace, then restore cursor position
function! <SID>StripTrailingWhitespaces()
  let l = line(".")
  let c = col(".")
  %s/\s\+$//e
  cal cursor(l, c)
endfun

" [ghcid]
" Start ghcid automatically. Only attempt to start it once.
let s:ghcid_started = 0
function! <SID>StartGhcid()
  if s:ghcid_started == 0
    let s:ghcid_started = 1
    if filereadable(".ghcid")
      exec "Ghcid"
    elseif filereadable("cabal.project")
      exec "Ghcid -c 'cabal new-repl'"
    elseif filereadable("stack.yaml")
      exec "Ghcid -c 'stack ghci'"
    elseif filereadable(expand('%'))
      exec "Ghcid -c 'ghci " . expand('%') . "'"
    endif
  endif
endfun

" qh behavior: move code to the left if there's room
function! <SID>QH()
  if getline('.')[0] == ' '
    exec "norm hm`0x``i\<Space>\<Esc>l"
  endif
endfunction

" q< behavior: shift code to the left if there's room
" FIXME: This just assumes shiftwidth=2 and ignores shiftround
function! <SID>Qlt()
  let l = getline('.')
  if l[0] == ' ' && l[1] == ' '
    exec "norm 2hm`02x``i\<Space>\<Space>\<Esc>l"
  endif
endfunction

function! <SID>EchoQuickFixEntry()
  let l:entries = getqflist()
  let l:bufnr = bufnr('%')
  let l:lnum = line('.')
  for e in entries
    if e.bufnr == l:bufnr && e.lnum == l:lnum
      echo e.text
      return
    endif
  endfor
endfunction

" ==============================================================================
" Autocommands
" ==============================================================================

" Jump to last cursor position on file open
au BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$") | exe "norm! g`\"" | endif

" Strip trailing whitespace on save
au BufWritePre * cal <SID>StripTrailingWhitespaces()

" Echo the quickfix entry on the current line, if any
au CursorMoved * cal <SID>EchoQuickFixEntry()

" ==============================================================================
" Abbreviations
" ==============================================================================

ia zalpha α
ia zbeta β
ia zchi χ
ia zdelta δ
ia zepsilon ε
ia zeta η
ia zgamma γ
ia ziota ι
ia zkappa κ
ia zlambda λ
ia zmu μ
ia znu ν
ia zomega ω
ia zphi φ
ia zpi π
ia zpsi ψ
ia zrho ρ
ia zsigma σ
ia ztau τ
ia ztheta θ
ia zupsilon υ
ia zxi ξ
ia zzeta ζ

ia zDelta Δ
ia zGamma Γ
ia zLambda Λ
ia zOmega Ω
ia zPhi Φ
ia zPi Π
ia zPsi Ψ
ia zSigma Σ
ia zTheta Θ
ia zXi Ξ

ia zforall ∀
ia zexists ∃
ia zbottom ⊥

ia zA 𝔸
ia zB 𝔹
ia zC ℂ
ia zD 𝔻
ia zE 𝔼
ia zF 𝔽
ia zG 𝔾
ia zH ℍ
ia zI 𝕀
ia zJ 𝕁
ia zK 𝕂
ia zL 𝕃
ia zM 𝕄
ia zN ℕ
ia zO 𝕆
ia zP ℙ
ia zQ ℚ
ia zR ℝ
ia zS 𝕊
ia zT 𝕋
ia zU 𝕌
ia zV 𝕍
ia zW 𝕎
ia zX 𝕏
ia zY 𝕐
ia zZ ℤ
ia zzgamma ℽ
ia zzGamma ℾ
ia zzpi ℼ
ia zzPi ℿ

ia zeq ≡
ia zne ≠
ia zle ≤
ia zge ≥
ia zdot ∘
ia znot ¬
ia zand ∧
ia zor ∨
ia zempty ∅
ia zunion ∪
ia zintersect ∩

" ------------------------------------------------------------------------------
" Command mode
" ------------------------------------------------------------------------------

" [UltiSnips]
" Edit snippets of current file type
ca snipedit UltiSnipsEdit

" ==============================================================================
" Plugin settings
" ==============================================================================

" [airline]
" let g:airline_symbols_ascii = 1
" let g:airline_theme='gruvbox'

" [AutoPairs]
" Which symbols are automatically paired
let g:AutoPairs = { '(': ')', '[': ']', '{': '}', "'": "'", '"': '"', '`': '`' }
let g:AutoPairsMapBS = 1 " Let AutoPairs handle <Backspace> (deletes pair)
let g:AutoPairsMapSpace = 1 " Let AutoPairs insert a space before the closing pair
" Don't handle <CR> in insert mode specially (cause it's brokenish)
let g:AutoPairsMapCR = 0
let g:AutoPairsCenterLine = 0
" Disable a bunch of random key bindings
let g:AutoPairsShortcutToggle = ''
let g:AutoPairsShortcutFastWrap = ''
let g:AutoPairsShortcutJump = ''
let g:AutoPairsShortcutBackInsert = ''
let g:AutoPairsMapCh = 0

" [EasyMotion]
let g:EasyMotion_do_mapping = 0 " Don't make any key mappings
let g:EasyMotion_smartcase = 1
let g:EasyMotion_use_upper = 1
let g:EasyMotion_keys = 'ASDGHKLQWERTYUIOPZXCVBNMFJ;'

" [elm]
let g:elm_setup_keybindings = 0 " Don't make any key mappings
let g:elm_format_autosave = 0 " Don't run elm-format on save

" [exchange]
" Don't make any key mappings
let g:exchange_no_mappings = 1

" [fzf]
" If the buffer is already open in another tab or window, jump to it rather
" than replace the current buffer (which would open 2 copies)
let g:fzf_buffers_jump = 1

" [ghcid]
" Don't pop up when there are warnings
let g:ghcid_background = 1

" [haskell]
" let g:haskell_indent_disable = 1
let g:haskell_enable_backpack = 1
let g:haskell_enable_pattern_synonyms = 1
let g:haskell_enable_quantification = 1
let g:haskell_enable_recursivedo = 1
let g:haskell_enable_typeroles = 1
let g:haskell_indent_bare_where = 0
let g:haskell_indent_before_where = 0
let g:haskell_indent_case = 2
let g:haskell_indent_if = 2
let g:haskell_indent_in = 0
let g:haskell_indent_let = 2
let g:haskell_indent_where = 0
" let g:haskell_classic_highlighting = 1

" [highlightedyank]
let g:highlightedyank_highlight_duration = 500 " highlight yank for 500ms
let g:highlightedyank_max_lines = 50

" [LanguageClient]
" " Specify the language-specific executables to run the LSP server
" let g:LanguageClient_serverCommands = {} " { 'haskell': ['hie-wrapper', '--lsp', '-d', '-l', '.HieWrapperLog'] }
" " Use global settings.json file
" let g:LanguageClient_settingsPath = "/home/mitchell/.config/lsp/settings.json"
" " LanguageClient doesn't seem to work very well, so verbosely log everything it
" " tries to do.
" let g:LanguageClient_loggingLevel = 'DEBUG'
" let g:LanguageClient_loggingFile = ".LanguageClientLog"

" [multiple-cursors]
let g:multi_cursor_use_default_mapping = 0
let g:multi_cursor_start_word_key = '<C-n>'
" let g:multi_cursor_select_all_word_key = '<A-n>'
" let g:multi_cursor_start_key = 'g<C-n>'
" let g:multi_cursor_select_all_key = 'g<A-n>'
let g:multi_cursor_next_key = '<C-n>'
let g:multi_cursor_prev_key = '<C-p>'
" let g:multi_cursor_skip_key = '<C-x>'
let g:multi_cursor_quit_key = '<Esc>'

" [NERDTree]
nn <silent> <Space>n :NERDTreeToggle<CR>

" [surround]
" Don't let surround provide any magic mappings
let g:surround_no_mappings = 1

" [UltiSnips]
let g:UltiSnipsUsePythonVersion = 3 " Tell UltiSnips to use Python 3 (in case auto-detect doesn't work)
let g:UltiSnipsSnippetsDir = "~/.config/nvim/UltiSnips" " Read snippets from this directory
let g:UltiSnipsEditSplit = 'horizontal' " Open snippets file with a horizontal split with :snipedit
" Unset annoying key mappings that can't be avoided
let g:UltiSnipsExpandTrigger="<C-j>"
let g:UltiSnipsListSnippets="<Nop>"
let g:UltiSnipsJumpForwardTrigger="<C-j>"
let g:UltiSnipsJumpBackwardTrigger="<C-k>"

" ==============================================================================
" Filetype-specific settings
" ==============================================================================

" [elm-vim]
" Space-p ("pretty ") to format Elm code
au FileType elm nn <buffer> <silent> <Space>p :ElmFormat<CR>

au FileType fzf,ghcid setl laststatus=0
  \| au BufLeave <buffer> setl laststatus=2
" Escape to quit little annoying temporary buffers
au FileType fzf,ghcid nn <silent> <buffer> <Esc> :q<CR>

" Swap ; and : in haskell
au FileType haskell ino ; :
au FileType haskell ino : ;
au FileType haskell nn r; r:
au FileType haskell nn r: r;
" au FileType haskell nn <Space>p :cal LanguageClient_textDocument_formatting()<CR>
" Start ghcid automatically
" au FileType haskell au BufWinEnter *.hs :cal <SID>StartGhcid()

" On <Enter>, go to error and close quickfix list
au FileType qf nn <silent> <buffer> <CR> <CR>:ccl<CR>

" ==============================================================================
" nvim-gtk settings
" ==============================================================================

if exists('g:GtkGuiLoaded')
  cal rpcnotify(1, 'Gui', 'Font', 'Hasklig 18')
endif

" ==============================================================================

" Notes to myself:
"
" 'o' in visual swaps cursor location
" g<C-a> in visual mode turns 1111 into 1234
" gi
" gv
