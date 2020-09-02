" Big default verb/action changes:
"
" * J = page down
" * K = page up
" * s = surround word
" * S = surround WORD
" * SS = surround line
" * U = redo
" * x = exchange
"
" TODO
" * shada?
" * Alt-K doesn't seem to work
" * Make 'sp' etc repeatable
" * Write function to make ds search for nearest surround and delete it
" * better function snippet (add args as I add types to type sig?)
" * lens snippet
" * prism snippet
" * replace easy-align

" ==============================================================================
" Plugins
" ==============================================================================

cal plug#begin(stdpath('data') . '/plugged')

Plug 'ElmCast/elm-vim', { 'for': 'elm' }
Plug 'LnL7/vim-nix', { 'for': 'nix' }
Plug 'RRethy/vim-illuminate' " Highlight occurrences of the word under the cursor
Plug 'Yggdroot/indentLine' " show markers every 2 columns of leading whitespace
Plug 'godlygeek/tabular' " Align on words
Plug 'itchyny/lightline.vim'
Plug 'junegunn/fzf.vim' " Fuzzy search source code, files, etc
Plug 'justinmk/vim-sneak' " two-letter f/t
Plug 'liuchengxu/vim-which-key' " thingy to tell me my own hotkeys (requires manual work)
Plug 'mcchrish/nnn.vim' " File browser thingy, kinda sucks, what's better?
Plug 'mengelbrecht/lightline-bufferline'
Plug 'mhinz/vim-signify' " Sign column
Plug 'mhinz/vim-startify' " Startup screen
Plug 'morhetz/gruvbox' " best color scheme
Plug 'neoclide/coc.nvim', { 'branch': 'release' }
Plug 'neovimhaskell/haskell-vim', { 'for': 'haskell' }
Plug 'psliwka/vim-smoothie' " Smooth paging up and down
Plug 'purescript-contrib/purescript-vim', { 'for': 'purescript' }
Plug 'rhysd/git-messenger.vim'
Plug 'romainl/vim-cool' " Automatically unhighlight when cursor moves
Plug 'romainl/vim-qf' " Vim quickfix improvements
Plug 'sdiehl/vim-ormolu', { 'for': 'haskell' }
Plug 'terryma/vim-multiple-cursors' " Multiple cursors for quick and dirty renaming
Plug 'tmsvg/pear-tree' " auto-pair function that claims to not suck; we'll see
Plug 'tommcdo/vim-exchange' " Swap the location of two selections
Plug 'tpope/vim-characterize' " Improved 'ga'
Plug 'tpope/vim-commentary' " Quick (un-)commenting
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-repeat' " Make '.' repeat more things out of the box
Plug 'tpope/vim-surround' " Some surround helpers
Plug 'unblevable/quick-scope' " Highlight the first, second, etc. instances of characters on a line
Plug 'vmchale/dhall-vim', { 'for': 'dhall' }
Plug 'wellle/targets.vim'
" https://github.com/voldikss/vim-floaterm/issues/163
" Plug 'voldikss/vim-floaterm' " floating terminal, almost good but nah

cal plug#end() " Automatically calls syntax on, filetype plugin indent on

" ==============================================================================
" Basic settings
" ==============================================================================

colo gruvbox

se bg=dark
se cb=unnamed,unnamedplus     " yank also copies to both clipboards
se cc=121                     " highlight column
se cul                        " higlight the current line
se et                         " convert tabs to spaces
se gp=rg\ --vimgrep           " use rg to grep
se hid                        " don't abandon out-of-sight buffers
se ic                         " case-insensitive searching
se icm=split                  " show live command substitutions
se lz                         " don't draw during e.g. applying a macro
se lbr                        " wrap lines in a more visually pleasing way
se lcs=tab:>\ ,trail:·,nbsp:+ " trailing whitespace markers
se list                       " show trailing whitespace, tabs, etc.
se nofen                      " never fold
se nojs                       " insert one space after ., ?, ! chars when joining
se noml                       " disable modelines
se nosmd                      " don't show mode, since lightline handle that
se nosol                      " don't jump cursor to start of line when moving
se nu                         " show line number gutter
se so=10                      " start scrolling before the cursor reaches the edge
se sr                         " shift to multiple of shiftwidth
se sw=2
se scl=yes                    " always draw signcolumn
se scs                        " don't ignore case if search contains uppercase char
se si                         " smart autoindenting when starting a new line
se smc=180                    " dont bother syntax-highlighting past this column
se stal=2                     " always show the tabline
se sts=2                      " tab key makes 2 spaces
se tgc
se title                      " put filename in window title
se tm=300                     " only wait this many ms for key sequence to complete
se udf                        " persist undo history across buffer exits
se ut=100                     " fire CursorHold after 100ms (default 4000ms)
se wmnu                       " complete commands with a little menu
se wim=list:longest,full      " wild menu completion behavior

" ==============================================================================
" Key mappings
" ==============================================================================

no ; :

" <Tab> to switch to the previously edited buffer
nn <Tab> <C-^>

" d. to delete the current line
nn d. ^D

" Prevent the cursor from jumping past a wrapped line when moving up and down
nn j gj
nn k gk

nn H ^
nn L $
ono H ^
ono L $
vn H ^
vn L g_

" Center after every search movement
nn n nzz
nn N Nzz
vn n nzz
vn N Nzz

nn r; r:
nn r: r;

" Disable annoying command search 'q:' that I never use
nn q: <Nop>

nn Q @q

" Make Y yank to the end of line, similar to how C and D behave
nn Y y$
" After visual mode yank, leave cursor at the end of the highlight
vn Y y`]

" U to redo. <C-r> comes from some plugin, maybe vim-repeat? (annoying)
nn U <C-r>
" Weaning myself of <C-R> to redo
nn <C-r> <Nop>

" Don't highlight matches *and* jump at the same time; only highlight
nn * *``
vn * y/<C-r>"<CR>
nn # #``

" Follow >>/<< shifted text around with the cursor
nm >> <Plug>MyNmapLl
nm << <Plug>MyNmapHh
" Get it to repeat with '.'
nn <silent> <Plug>MyNmapLl >>ll:cal repeat#set("\<Plug>MyNmapLl")<CR>
nn <silent> <Plug>MyNmapHh <<hh:cal repeat#set("\<Plug>MyNmapHh")<CR>

" ,j to join (since J moves down)
nn ,j m`J``

" Ctrl+S to search-and-replace in the file
nn <C-s> :%s//g<Left><Left>
xn <C-s> :s//g<Left><Left>

" Move buffers with Ctrl+jk
nn <silent> <C-j> :bn<CR>
nn <silent> <C-k> :bp<CR>

" Delete the current buffer with Space-d, or quit vim if it's the only buffer
nn <expr> <silent> <Space>d len(getbufinfo({'buflisted': 1})) ==? 1 ? ":q\<CR>" : ":bd\<CR>"

" Move vertical splits with Ctrl+hl (sorry, horizontal splits)
" I never use vertical splits anyway so these could be repurposed.
nn <C-h> <C-w>h
nn <C-l> <C-w>l

" github.com/mitchellwrosen/repld stuff
nn <silent> <Space>s m`vip<Esc>:silent '<,'>w !repld-send --no-echo<CR>``
nn <silent> <Space>S m`:silent w !repld-send<CR>``
vn <silent> <Space>s m`<Esc>:silent '<,'>w !repld-send<CR>``

ino ; :
ino : ;

" <C-v> to paste from * register
ino <C-v> <C-r>*

" Ctrl+space for omnicomplete
im <C-Space> <C-x><C-o>

" When a popup menu is visible, move thru it with tab and select with enter
" ino <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
ino <expr> <Tab> pumvisible() ? "\<C-n>" : coc#refresh()
ino <expr> <CR> pumvisible() ? "\<C-y>" : "\<CR>"

ino <C-u> <Nop>

" ------------------------------------------------------------------------------
" Terminal mode
" ------------------------------------------------------------------------------

" tno <A-[> <Esc>

" ==============================================================================
" Commands
" ==============================================================================

" command! Vimrc e "~/.config/nvim/init.vim"

" ==============================================================================
" Functions
" ==============================================================================

function! s:GetSelectedText() abort
  let [l:lnum1, l:col1] = getpos("'<")[1:2]
  let [l:lnum2, l:col2] = getpos("'>")[1:2]
  if &selection ==# 'exclusive'
    let l:col2 -= 1
  endif
  let l:lines = getline(l:lnum1, l:lnum2)
  let l:lines[-1] = l:lines[-1][:l:col2 - 1]
  let l:lines[0] = l:lines[0][l:col1 - 1:]
  return l:lines
endfunction

" Remove trailing whitespace, then restore cursor position
function! <SID>StripTrailingWhitespaces()
  let l = line('.')
  let c = col('.')
  %s/\s\+$//e
  cal cursor(l, c)
endfun

" function! <SID>EchoQuickFixEntry()
"   let entries = getqflist()
"   let bufnr = bufnr('%')
"   let lnum = line('.')
"   for e in entries
"     if e.bufnr == bufnr && e.lnum == lnum
"       echo e.text
"       return
"     endif
"   endfor
" endfunction

let s:mitchell_term_bufid = v:null
let g:mitchell_term_jobid = v:null
let s:mitchell_term_winid = v:null

let s:mitchell_term_opts = {}
function! s:mitchell_term_opts.on_exit(jobid, data, event) abort
  " If the terminal process exits before the buffer, close the buffer, too
  if bufexists(s:mitchell_term_bufid)
    execute 'bw!' s:mitchell_term_bufid
  endif
  let g:mitchell_term_jobid = v:null
endfunction

function! MitchellTerm()
  " We're in the terminal, so toggling it would require knowing some other
  " window to jump to. Too much work.
  if nvim_get_current_buf() ==# s:mitchell_term_bufid
    return

  " Terminal window exists (even though it might be showing some other buffer)
  elseif s:mitchell_term_winid !=# v:null && winbufnr(s:mitchell_term_winid) !=# -1
    call nvim_win_close(s:mitchell_term_winid, v:false)

  else
    " let editableWidth = s:GetEditableWidth()
    let opts = {}
    " let opts.col = 120 + winwidth(0) - editableWidth
    let opts.col = winwidth(0) - 80
    let opts.height = line('w$') - line('w0') + 1
    let opts.relative = 'editor'
    let opts.row = 1
    let opts.style = 'minimal'
    " let opts.width = editableWidth - 120
    let opts.width = 80

    " Terminal window doesn't exist, but terminal buffer does
    if bufexists(s:mitchell_term_bufid)
      let s:mitchell_term_winid = nvim_open_win(s:mitchell_term_bufid, v:false, opts)
      "
    " Neither terminal window nor buffer exist
    else
      let s:mitchell_term_bufid = nvim_create_buf(v:false, v:true)
      let winid = nvim_get_current_win()
      let s:mitchell_term_winid = nvim_open_win(s:mitchell_term_bufid, v:true, opts)
      let g:mitchell_term_jobid = termopen(&shell, s:mitchell_term_opts)
      call nvim_win_set_option(s:mitchell_term_winid, 'winbl', 20)
      call win_gotoid(winid)
    endif
  endif
endfunction

" Compute the width of the editable part of the screen
function! s:GetEditableWidth()
  redir => x
    exe "sil sign place buffer=" . nvim_get_current_buf()
  redir end
  let signlist = split(x, '\n')
  return winwidth(0) - ((&number || &relativenumber) ? &numberwidth : 0) - &foldcolumn - (len(signlist) > 1 ? 2 : 0)
endfunction

function! s:MitchellTermSendSelection() abort
  if g:mitchell_term_jobid !=# v:null
    let lines = s:GetSelectedText()
    return chansend(g:mitchell_term_jobid, lines + [""])
  else
    return 0
  endif
endfunction

command! -range MitchellTermSendSelection call s:MitchellTermSendSelection()

nn <silent> <Space>tt :call MitchellTerm()<CR>
nn <silent> <Space>ts m`vip<Esc>:'<,'>MitchellTermSendSelection<CR>``
vn <silent> <Space>ts :MitchellTermSendSelection<CR>

" ==============================================================================
" Autocommands
" ==============================================================================

aug mitchellwrosen
  autocmd!
aug END

" Jump to last cursor position on file open
au mitchellwrosen BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$") | exe "norm! g`\"" | endif

" Strip trailing whitespace on save
au mitchellwrosen BufWritePre * cal <SID>StripTrailingWhitespaces()

" On <Enter>, go to error and close quickfix list
au mitchellwrosen FileType qf nn <silent> <buffer> <CR> <CR>:ccl<CR>

au mitchellwrosen FileType unison setlocal commentstring=--\ %s

" Esc escapes terminal mode
au mitchellwrosen TermOpen * tno <buffer> <Esc> <C-\><C-n>
" forcibly exit a terminal buffer, because there's nothing to save
au mitchellwrosen TermOpen * nn <silent> <buffer> <Space>d :bw!<CR>

" Briefly highlight yanks
au mitchellwrosen TextYankPost * silent! lua vim.highlight.on_yank {higroup="IncSearch", timeout=500}

" Echo the quickfix entry on the current line, if any
" au CursorMoved * cal <SID>EchoQuickFixEntry()

" ==============================================================================
" Plugin settings
" ==============================================================================

" [ElmCast/elm-vim]
let g:elm_setup_keybindings = 0 " Don't make any key mappings
let g:elm_format_autosave = 1 " Run elm-format on save

" [godlygeek/tabular]
" Space-a to align on the word under the cursor
nn <silent> <Space>a m`:exe "Tabularize /" . expand("<cWORD>")<CR>``

" [junegunn/fzf.vim]
" If the buffer is already open in another tab or window, jump to it rather
" than replace the current buffer (which would open 2 copies)
let g:fzf_buffers_jump = 1
let g:fzf_layout = { 'window': { 'height': 0.9, 'width': 0.9 } }

" [junegunn/fzf.vim]
" Space-o ("open") to fuzzy file search, both git- and everything-variants
nn <expr> <Space>o (len(system('git rev-parse')) ? ':Files' : ':GFiles')."\<CR>"
" Space-f ("find") the word under the cursor
nn <Space>f :Rg <C-r><C-w><CR>
" Would be nice to do this without yanking?
vn <Space>f y:Rg <C-r>"<CR>
" Space-k (because it's a home-row key) to fuzzy-search buffers
nn <Space>k :Buffers<CR>
" Space-h to see the git history of the current file
nn <Space>h :BCommits<CR>

command! -bar BCommits call fzf#vim#buffer_commits(1)

command! -bar -nargs=? -complete=buffer Buffers
  \ call fzf#vim#buffers(
  \   <q-args>,
  \   fzf#vim#with_preview({'options': ['--info=inline', '--layout=reverse']}, 'down:60%'),
  \   0)

command! -nargs=? -complete=dir Files
  \ call fzf#vim#files(
  \   <q-args>,
  \   fzf#vim#with_preview({'options': ['--info=inline', '--layout=reverse']}, 'down:60%'),
  \   0)

command! -nargs=? GFiles
  \ call fzf#vim#gitfiles(
  \   <q-args>,
  \   fzf#vim#with_preview({'options': ['--info=inline', '--layout=reverse']}, 'down:60%'),
  \   0)

" Would be nice if '-1' worked here https://github.com/junegunn/fzf/issues/1750
" function! <SID>Rg(query)
"   let command_fmt = 'rg --column --line-number --no-heading --color=always -- %s || true'
"   let initial_command = printf(command_fmt, shellescape(a:query))
"   let reload_command = printf(command_fmt, '{q}')
"   let spec = {'options': ['-0', '-1', '--phony', '--query', a:query, '--bind', 'change:reload:'.reload_command]}
"   call fzf#vim#grep(initial_command, 1, fzf#vim#with_preview(spec), 1)
" endfunction
" command! -nargs=* Rg call <SID>Rg(<q-args>)

command! -nargs=* Rg
  \ call fzf#vim#grep(
  \   'rg --column --line-number --no-heading --color=always -- '.shellescape(<q-args>),
  \   1,
  \   fzf#vim#with_preview({'options': ['--border', '--info=inline', '--layout=reverse']}, 'down:60%'),
  \   0)

command! -nargs=* Rgu
  \ call fzf#vim#grep(
  \   'rg --line-number --multiline --multiline-dotall --no-heading --color=always -- '.shellescape(<q-args>),
  \   0,
  \   fzf#vim#with_preview({'options': ['--border', '--info=inline', '--layout=reverse']}, 'down:60%'),
  \   0)

au mitchellwrosen FileType fzf setl laststatus=0
  \| au BufLeave <buffer> setl laststatus=2
" Escape to quit little annoying temporary buffers
au mitchellwrosen FileType fzf nn <silent> <buffer> <Esc> :q<CR>
" Unmap Esc quitting terminal mode, so fzf handles it (result: one Esc closes fzf)
au mitchellwrosen FileType fzf tunma <buffer> <Esc>

" <Space>ff to find-function (ag can match over multiple lines)
" <Space>ft to find-type (ripgrep is faster)
au mitchellwrosen FileType haskell nn <buffer> <Space>ff :Ag (<Bslash>b)<C-r><C-w><Bslash>b[ <Bslash>t<Bslash>n]+::<CR>
au mitchellwrosen FileType haskell nn <buffer> <Space>ft :Rg (((data<Bar>newtype<Bar>type)\s+)<Bar>class .*)\b<C-r><C-w>\b<CR>
au mitchellwrosen FileType haskell nn <buffer> <Space>fa :Rgu (<C-r><C-w>\b\s+::)<Bar>((data(\sfamily)?<Bar>newtype<Bar>type(\sfamily)?)\s+<C-r><C-w>\b)<Bar>(class\s+(\(.*\)\s+=>\s+)?<C-r><C-w>\b\s+where)<CR>

" [itchyny/lightline.vim]
function! LightlineFilename()
  let filename = expand('%:t')
  let modified = &modified ? '+' : ''
  return filename . modified
endfunction

let g:lightline = {}
let g:lightline.active = {}
let g:lightline.active.left = [ [ 'mode', 'paste' ], [ 'branch' ] ]
let g:lightline.active.right = [ [ 'lineinfo' ], [ 'percent' ], [ 'filetype' ] ]
let g:lightline.colorscheme = 'gruvbox'
let g:lightline.component_expand = {}
let g:lightline.component_expand.buffers = 'lightline#bufferline#buffers'
let g:lightline.component_function = {}
let g:lightline.component_function.branch = 'FugitiveHead'
let g:lightline.component_function.filename = 'LightlineFilename'
let g:lightline.component_type = {}
let g:lightline.component_type.buffers = 'tabsel'
let g:lightline.mode_map = {
      \ 'C': '𝒸ℴ𝓂𝓂𝒶𝓃𝒹',
      \ 'i': '𝒾𝓃𝓈ℯ𝓇𝓉',
      \ 'n': '𝓃ℴ𝓇𝓂𝒶ℓ',
      \ 'R': '𝓇ℯ𝓅ℓ𝒶𝒸ℯ',
      \ 'v': '𝓋𝒾𝓈𝓊𝒶ℓ',
      \ 'V': '𝓋𝒾𝓈𝓊𝒶ℓ–ℓ𝒾𝓃ℯ',
      \ "\<C-v>": '𝓋𝒾𝓈𝓊𝒶ℓ–𝒷ℓℴ𝒸𝓀',
      \ }
let g:lightline.tab = {}
let g:lightline.tab.active = [ 'tabnum', 'filename', 'modified' ]
let g:lightline.tab.inactive = [ 'tabnum', 'filename', 'modified' ]
let g:lightline.tabline = {}
let g:lightline.tabline.left = [ [ 'buffers' ] ]
let g:lightline.tabline.right = [ [ ] ]

" [justinmk/vim-sneak]
" "clever" sneak - pressing z without moving the cursor will move to the next match
let g:sneak#s_next = 1
" Disable highlighting
hi! link Sneak None

nm : <Plug>Sneak_;
nm f <Plug>Sneak_f
nm F <Plug>Sneak_F
nm t <Plug>Sneak_t
nm t <Plug>Sneak_T
nm z <Plug>Sneak_s
nm Z <Plug>Sneak_S
vm z <Plug>Sneak_s
vm Z <Plug>Sneak_S
om z <Plug>Sneak_s
om Z <Plug>Sneak_S

" [liuchengxu/vim-which-key]
let g:which_key_use_floating_win = 1
let g:which_key_hspace = 1
cal which_key#register('<Space>', 'g:which_key_map_space')
let g:which_key_map_space = {
      \ 'a': 'align',
      \ 'b': 'git-blame',
      \ 'd': 'delete-buffer',
      \ 'f': 'find',
      \ 'h': 'history',
      \ 'k': 'find-buffer',
      \ 'm': 'comment',
      \ 'n': 'file-browser',
      \ 'o': 'find-file',
      \ 'S': 'repld-send-buffer',
      \ 's': 'repld-send',
      \ }
cal which_key#register('?', 'g:which_key_map_question')
" Why are some buffer things broken?
let g:which_key_map_question = {
      \ 'b':
      \   { 'name': '+buffer',
      \     'f': ['call feedkeys("<Space>k")', 'find-buffer (<Space>k)'],
      \     'p': ['<C-k>', 'prev-buffer (<C-k>)'],
      \     'n': ['<C-j>', 'next-buffer (<C-j>)'],
      \     'd': ['<Space>d', 'delete-buffer (<Space>d)'],
      \     's': ['<Tab>', 'swap-buffer (<Tab>)'],
      \   },
      \ 'e':
      \   { 'name': '+edit',
      \     'j': ['<C-s>', 'find-and-replace (<C-s>)'],
      \     'x':
      \       { 'name': '+exchange',
      \         'c': ['call feedkeys("xc")', 'exchange-clear (xc)'],
      \         'l': ['call feedkeys("xx")', 'exchange-line (xx)'],
      \         'w': ['call feedkeys("xw")', 'exchange-word (xw)'],
      \       },
      \   },
      \ 'w':
      \   { 'name': '+window',
      \     'h': ['<C-h>', 'window-left (<C-h>)'],
      \     'l': ['<C-l>', 'window-right (<C-l>)'],
      \   },
      \ 'Q': ['call feedkeys("@q")', 'execute-macro-q'],
      \ }

nn <silent> <Space> :WhichKey '<Space>'<CR>
vn <silent> <Space> :WhichKeyVisual '<Space>'<CR>
nn <silent> ? :WhichKey '?'<CR>

" [mcchrish/nnn.vim]
let g:nnn#set_default_mappings = 0
let g:nnn#command = 'nnn -c -n'
let g:nnn#layout = { 'window': { 'height': 0.9, 'width': 0.5, 'xoffset': 0.99 }}

" [mengelbrecht/lightline-bufferline]
let g:lightline#bufferline#modified = '+'

" [morhetz/gruvbox]
let g:gruvbox_italic = 1 " enable italics
let g:gruvbox_improved_strings = 1 " thought this was supposed to extra-highlight strings?

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

" [neoclide/coc.nvim]
" <Left>/<Right> to jump around warnings/errors (annoying that it's only buffer-local)
nm <silent> <Left> <Plug>(coc-diagnostic-prev)
nm <silent> <Right> <Plug>(coc-diagnostic-next)
" gd to go to definition of thing under cursor
" Also <Del> (trying it out since it's one key)
nm <silent> gd <Plug>(coc-definition)
nm <silent> <Del> <Plug>(coc-definition)
" <Enter> to show type of thing under cursor
nn <silent> <Enter> :call <SID>HandleEnter()<CR>
" <Space>i to open quickfix
nn <silent> <Space>i :CocFix<CR>
" Backspace to open all warnings/errors in a list
nn <silent> <BS> :CocList diagnostics<CR>

function! s:HandleEnter()
  if coc#util#has_float()
    call coc#util#float_hide()
  else
    call CocAction('doHover')
  endif
endfunction

" [neovimhaskell/haskell-vim]
let g:haskell_indent_disable = 1
let g:haskell_enable_backpack = 1
let g:haskell_enable_pattern_synonyms = 1
let g:haskell_enable_quantification = 1
let g:haskell_enable_recursivedo = 1
let g:haskell_enable_typeroles = 1

" [romainl/vim-qf]
" Toggle the quickfix ("location") menu; move thru quickfix items with Alt+jk
" Hmm... I never seem to use these... do they even work? Wtf is quickfix?
nm <Space>l <Plug>(qf_qf_toggle)
nm <A-j> <Plug>(qf_qf_next)
nm <A-k> <Plug>(qf_qf_prev)

" [sdiehl/vim-ormolu]
let g:ormolu_disable = 1

au mitchellwrosen FileType haskell nn <buffer> <silent> <Space>p :call RunOrmolu()<CR>

" [mcchrish/nnn.vim]
nn <silent> <Space>n :NnnPicker<CR>

" [mhinz/signify]
let g:signify_sign_change = 'Δ'
let g:signify_sign_delete = '-'
" I only use git, so only bother integrating with it (performance win!)
let g:signify_vcs_list = [ 'git' ]

" [mhinz/vim-startify]
let g:startify_custom_footer = ['   [e]  empty buffer', '   [q]  quit']
let g:startify_custom_header = []
let g:startify_custom_indices = ['a', 's', 'd', 'f', 'l', 'g', 'h', 'w', 'r', 'u', 'o', 'p', 't', 'y', 'z', 'x', 'c', 'v', 'm', ',', '.', '/', 'b', 'n', '1', '2', '3', '4', '5', '6']
let g:startify_enable_special = 0
" faster startup
let g:startify_enable_unsafe = 1
let g:startify_files_number = 30
let g:startify_lists = [{ 'type': 'files' }]
let g:startify_relative_path = 1

" make floaterm not leave an empty buffer in startify
" au User Startified setlocal buflisted

" In startify screen, undo my j=gj, k=gk mappings, because they press 'g'
au mitchellwrosen FileType startify nn <buffer> j j
au mitchellwrosen FileType startify nn <buffer> k k

" [psliwka/vim-smoothie]
let g:smoothie_base_speed = 15
let g:smoothie_no_default_mappings = 1
let g:smoothie_update_interval = 10

" very unfortunate: vm variants don't work here...
nm J <Plug>(SmoothieForwards)
nm K <Plug>(SmoothieBackwards)
vn J <C-D>
vn K <C-U>

" [rhysd/git-messenger.vim]
let g:git_messenger_always_into_popup = v:true
let g:git_messenger_extra_blame_args = '-w'
let g:git_messenger_no_default_mappings = v:true

" blame the line under the cursor
nm <Space>b <Plug>(git-messenger)

function! <SID>init_gitmessengerpopup() abort
  nm <buffer><Enter> q
  nm <buffer><Esc> q
  nm <buffer>h o
  nm <buffer>l O
endfunction
autocmd mitchellwrosen FileType gitmessengerpopup call <SID>init_gitmessengerpopup()

" [RRethy/vim-illuminate]
" highlight immediately
let g:Illuminate_delay = 0
" don't highlight the word under the cursor
let g:Illuminate_highlightUnderCursor = 0

" [tmsvg/pear-tree]
" look around to better balance parens rather than just always pairing
let g:pear_tree_smart_openers = 1
let g:pear_tree_smart_closers = 1
let g:pear_tree_smart_backspace = 1

" [tommcdo/vim-exchange]
" Don't make any key mappings
let g:exchange_no_mappings = 1

" x ("exchange") once to yank, x again to exchange with the first yank
nm x <Plug>(Exchange)
" Manually make [exhange] replace 'w' with 'e', as vim does for e.g. 'c'
nm xw <Plug>(Exchange)e
nm xW <Plug>(Exchange)E
" xx to exchange-yank the whole line (and return cursor to where it was)
nm xx m`<Plug>(ExchangeLine)``
" xc to clear the exchange
nm xc <Plug>(ExchangeClear)
vm x <Plug>(Exchange)

" [tpope/vim-commentary]
" Toggle comment
nm <Space>m <Plug>CommentaryLine
vm <Space>m <Plug>Commentary

" [tpope/vim-surround]
" Don't let surround provide any magic mappings
let g:surround_no_mappings = 1

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
nm ds<Space> mz<Plug>Dsurround <Space>`zh
nm s' mz<Plug>Csurround w'`zl
nm s" mz<Plug>Csurround w"`zl
nm s( mz<Plug>Csurround w)`zl
nm s[ mz<Plug>Csurround w]`zl
nm s{ mz<Plug>Csurround w}`zl
nm sp mz<Plug>Csurround w)`zl
nm s<Space> mz<Plug>Csurround w <Space>`zl
nm S' mz<Plug>Csurround W'`zl
nm S" mz<Plug>Csurround W"`zl
nm S( mz<Plug>Csurround W)`zl
nm S[ mz<Plug>Csurround W]`zl
nm S{ mz<Plug>Csurround W}`zl
nm Sp mz<Plug>Csurround W)`zl
nm S<Space> mz<Plug>Csurround W <Space>`zl
nm SS' mz<Plug>Yssurround'`z
nm SS" mz<Plug>Yssurround"`z
nm SS( mz<Plug>Yssurround)`z
nm SS[ mz<Plug>Yssurround]`z
nm SS{ mz<Plug>Yssurround}`z
nm SSp mz<Plug>Yssurround)`z
vm s' <Plug>VSurround'
vm s" <Plug>VSurround"
vm s( <Plug>VSurround)
vm s[ <Plug>VSurround]
vm s{ <Plug>VSurround}
vm sp <Plug>VSurround)

" [unblevable/quick-scope]
" let g:qs_lazy_highlight = 1 " only kick in after updatetime ms
let g:qs_max_chars = 120

" [voldikss/vim-floaterm]
" let g:floaterm_autoclose = 2
" let g:floaterm_title = ''
" nn <Space>tl :FloatermNew --height=0.9 --position=right --width=0.5<CR>
" nn <silent> <Space>tt :Tt --height=0.9 --position=right --width=0.5<CR>

" au mitchellwrosen FileType floaterm nn <buffer> <silent> <C-j> :FloatermNext<CR>
" au mitchellwrosen FileType floaterm nn <buffer> <silent> <C-k> :FloatermPrev<CR>

" [wellle/targets.vim]
nm cil9 cil)
nm cil0 cil)
nm cin9 cin)
nm cin0 cin)

" [Yggdroot/indentLine]
let g:indentLine_color_term = 239
let g:indentLine_char = '┊'

" ==============================================================================
" nvim-gtk settings
" ==============================================================================

if exists('g:GtkGuiLoaded')
  cal rpcnotify(1, 'Gui', 'Font', 'PragmataPro Mono Liga 18')
endif

" ==============================================================================

" Notes to myself:
"
" 'o' in visual swaps cursor location
" g<C-a> in visual mode turns 1\n1\n1\n1 into 2\n3\n4\n5
" gi
" gv

" ==============================================================================
" Unicode goodies
" ==============================================================================

"       ▼  Controls and Latin-1 Suppl.
"  U+00A0    ¡ ¢ £ ¤ ¥ ¦ § ¨ © ª « ¬ ­ ® ¯

ino <C-u>! ¡
ino <C-u>cent ¢
ino <C-u>pound £
ino <C-u>currency ¤
ino <C-u>yen ¥
ino <C-u>brokenbar ¦
ino <C-u>section §
ino <C-u>diaeresis ¨
ino <C-u>copyright ©
ino <C-u>fordinal ª
ino <C-u><<" «
ino <C-u>not ¬
ino <C-u>softhyphen ­
ino <C-u>registered ®
ino <C-u>macron ¯

"  U+00B0  ° ± ² ³ ´ µ ¶ · ¸ ¹ º » ¼ ½ ¾ ¿

ino <C-u>degree °
ino <C-u>plusminus ±
ino <C-u>^2 ²
ino <C-u>^3 ³
ino <C-u>` ´
ino <C-u>micro µ
ino <C-u>pilcrow ¶
ino <C-u>middledot ·
ino <C-u>cedilla ¸
ino <C-u>^1 ¹
ino <C-u>mordinal º
ino <C-u>>>" »
ino <C-u>1/4 ¼
ino <C-u>1/2 ½
ino <C-u>3/4 ¾
ino <C-u>? ¿

"  U+00C0  À Á Â Ã Ä Å Æ Ç È É Ê Ë Ì Í Î Ï

" ino <C-u>graveA À
" ino <C-u>acuteA Á
" ino <C-u>circumflexA Â
" ino <C-u>tildeA Ã
" ino <C-u>diaeresisA Ä
" ino <C-u>ringA Å
" ino <C-u>AE Æ
" ino <C-u>cedillaC Ç
" ino <C-u>graveE È
" ino <C-u>acuteE É
" ino <C-u>circumflexE Ê
" ino <C-u>diaeresisE Ë
" ino <C-u>graveI Ì
" ino <C-u>acuteI Í
" ino <C-u>circumflexI Î
" ino <C-u>diaeresisI Ï

"  U+00D0  Ð Ñ Ò Ó Ô Õ Ö × Ø Ù Ú Û Ü Ý Þ ß

" ino <C-u>Eth Ð
" ino <C-u>tildeN Ñ
" ino <C-u>graveO Ò
" ino <C-u>acuteO Ó
" ino <C-u>circumflexO Ô
" ino <C-u>tildeO Õ
" ino <C-u>diaeresisO Ö
ino <C-u>mult ×
" ino <C-u>strokeO Ø
" ino <C-u>graveU Ù
" ino <C-u>acuteU Ú
" ino <C-u>circumflexU Û
" ino <C-u>diaeresisU Ü
" ino <C-u>acuteY Ý
" ino <C-u>Thorn Þ
" ino <C-u>eszett ß

"  U+00E0  à á â ã ä å æ ç è é ê ë ì í î ï

" ino <C-u>gravea à
" ino <C-u>acutea á
" ino <C-u>circumflexa â
" ino <C-u>tildea ã
" ino <C-u>diaeresisa ä
" ino <C-u>ringa å
ino <C-u>ae æ
" ino <C-u>cedillac ç
" ino <C-u>gravee è
" ino <C-u>acutee é
" ino <C-u>circumflexe ê
" ino <C-u>diaeresise ë
" ino <C-u>gravei ì
" ino <C-u>acutei í
" ino <C-u>circumflexi î
" ino <C-u>diaeresisi ï

"  U+00F0  ð ñ ò ó ô õ ö ÷ ø ù ú û ü ý þ ÿ

" ino <C-u>eth ð
" ino <C-u>tilden ñ
" ino <C-u>graveo ò
" ino <C-u>acuteo ó
" ino <C-u>circumflexo ô
" ino <C-u>tildeo õ
" ino <C-u>diaeresiso ö
ino <C-u>div ÷
" ino <C-u>strokeo ø
" ino <C-u>graveu ù
" ino <C-u>acuteu ú
" ino <C-u>circumflexu û
" ino <C-u>diaeresisu ü
" ino <C-u>acutey ý
" ino <C-u>thorn þ
" ino <C-u>diaeresisy ÿ

"       ▼  Latin Extended-A
"  U+0100  Ā ā Ă ă Ą ą Ć ć Ĉ ĉ Ċ ċ Č č Ď ď

" ino <C-u>macronA Ā
" ino <C-u>macrona ā
" ino <C-u>breveA Ă
" ino <C-u>brevea ă
" ino <C-u>ogonekA Ą
" ino <C-u>ogoneka ą
" ino <C-u>acuteC Ć
" ino <C-u>acutec ć
" ino <C-u>circumflexC Ĉ
" ino <C-u>circumflexc ĉ
" ino <C-u>dotC Ċ
" ino <C-u>dotc ċ
" ino <C-u>caronC Č
" ino <C-u>caronc č
" ino <C-u>caronD Ď
" ino <C-u>carond ď

"  U+0110  Đ đ Ē ē Ĕ ĕ Ė ė Ę ę Ě ě Ĝ ĝ Ğ ğ

" ino <C-u>strokeD Đ
" ino <C-u>stroked đ
" ino <C-u>macronE Ē
" ino <C-u>macrone ē
" ino <C-u>breveE Ĕ
" ino <C-u>brevee ĕ
" ino <C-u>dotE Ė
" ino <C-u>dote ė
" ino <C-u>ogonekE Ę
" ino <C-u>ogoneke ę
" ino <C-u>caronE Ě
" ino <C-u>carone ě
" ino <C-u>circumflexG Ĝ
" ino <C-u>circumflexg ĝ
" ino <C-u>breveG Ğ
" ino <C-u>breveg ğ

"  U+0120  Ġ ġ Ģ ģ Ĥ ĥ Ħ ħ Ĩ ĩ Ī ī Ĭ ĭ Į į

" ino <C-u>dotG Ġ
" ino <C-u>dotg ġ
" ino <C-u>cedillaG Ģ
" ino <C-u>cedillag ģ
" ino <C-u>circumflexH Ĥ
" ino <C-u>circumflexh ĥ
" ino <C-u>strokeH Ħ
" ino <C-u>strokeh ħ
" ino <C-u>tildeI Ĩ
" ino <C-u>tildei ĩ
" ino <C-u>macronI Ī
" ino <C-u>macroni ī
" ino <C-u>breveI Ĭ
" ino <C-u>brevei ĭ
" ino <C-u>ogonekI Į
" ino <C-u>ogoneki į

"  U+0130  İ ı Ĳ ĳ Ĵ ĵ Ķ ķ ĸ Ĺ ĺ Ļ ļ Ľ ľ Ŀ

" ino <C-u>dotI İ
" ino <C-u>dotlessi ı
" ino <C-u>IJ Ĳ
" ino <C-u>ij ĳ
" ino <C-u>circumflexJ Ĵ
" ino <C-u>circumflexj ĵ
" ino <C-u>cedillaK Ķ
" ino <C-u>cedillak ķ
" ino <C-u>kra ĸ
" ino <C-u>acuteL Ĺ
" ino <C-u>acutel ĺ
" ino <C-u>cedillaL Ļ
" ino <C-u>cedillal ļ
" ino <C-u>caronL Ľ
" ino <C-u>caronl ľ
" ino <C-u>middledotL Ŀ

"  U+0140  ŀ Ł ł Ń ń Ņ ņ Ň ň ŉ Ŋ ŋ Ō ō Ŏ ŏ

" ino <C-u>middledotl ŀ
" ino <C-u>strokeL Ł
" ino <C-u>strokel ł
" ino <C-u>acuteN Ń
" ino <C-u>acuten ń
" ino <C-u>cedillaN Ņ
" ino <C-u>cedillan ņ
" ino <C-u>caronN Ň
" ino <C-u>caronn ň
" ino <C-u>apostrophen ŉ
" ino <C-u>Eng Ŋ
" ino <C-u>eng ŋ
" ino <C-u>macronO Ō
" ino <C-u>macrono ō
" ino <C-u>breveO Ŏ
" ino <C-u>breveo ŏ

"  U+0150  Ő ő Œ œ Ŕ ŕ Ŗ ŗ Ř ř Ś ś Ŝ ŝ Ş ş

" ino <C-u>dacuteO Ő
" ino <C-u>dacuteo ő
" ino <C-u>OE Œ
" ino <C-u>oe œ
" ino <C-u>acuteR Ŕ
" ino <C-u>acuter ŕ
" ino <C-u>cedillaR Ŗ
" ino <C-u>cedillar ŗ
" ino <C-u>caronR Ř
" ino <C-u>caronr ř
" ino <C-u>acuteS Ś
" ino <C-u>acutes ś
" ino <C-u>circumflexS Ŝ
" ino <C-u>circumflexs ŝ
" ino <C-u>cedillaS Ş
" ino <C-u>cedillas ş

"  U+0160  Š š Ţ ţ Ť ť Ŧ ŧ Ũ ũ Ū ū Ŭ ŭ Ů ů

" ino <C-u>caronS Š
" ino <C-u>carons š
" ino <C-u>cedillaT Ţ
" ino <C-u>cedillat ţ
" ino <C-u>caronT Ť
" ino <C-u>caront ť
" ino <C-u>strokeT Ŧ
" ino <C-u>stroket ŧ
" ino <C-u>tildeU Ũ
" ino <C-u>tildeu ũ
" ino <C-u>macronU Ū
" ino <C-u>macronu ū
" ino <C-u>breveU Ŭ
" ino <C-u>breveu ŭ
" ino <C-u>ringU Ů
" ino <C-u>ringu ů

"  U+0170  Ű ű Ų ų Ŵ ŵ Ŷ ŷ Ÿ Ź ź Ż ż Ž ž ſ

" ino <C-u>dacuteU Ű
" ino <C-u>dacuteu ű
" ino <C-u>ogonekU Ų
" ino <C-u>ogoneku ų
" ino <C-u>circumflexW Ŵ
" ino <C-u>circumflexw ŵ
" ino <C-u>circumflexY Ŷ
" ino <C-u>circumflexy ŷ
" ino <C-u>diaeresisY Ÿ
" ino <C-u>acuteZ Ź
" ino <C-u>acutez ź
" ino <C-u>dotZ Ż
" ino <C-u>dotz ż
" ino <C-u>caronZ Ž
" ino <C-u>caronz ž
" ino <C-u>shorts ſ

"       ▼  Latin Extended-B
"  U+0180  ƀ Ɓ Ƃ ƃ Ƅ ƅ Ɔ Ƈ ƈ Ɖ Ɗ Ƌ ƌ ƍ Ǝ Ə
"  U+0190  Ɛ Ƒ ƒ Ɠ Ɣ ƕ Ɩ Ɨ Ƙ ƙ ƚ ƛ Ɯ Ɲ ƞ Ɵ
"  U+01A0  Ơ ơ Ƣ ƣ Ƥ ƥ Ʀ Ƨ ƨ Ʃ ƪ ƫ Ƭ ƭ Ʈ Ư
"  U+01B0  ư Ʊ Ʋ Ƴ ƴ Ƶ ƶ Ʒ Ƹ ƹ ƺ ƻ Ƽ ƽ ƾ ƿ
"  U+01C0  ǀ ǁ ǂ ǃ Ǆ ǅ ǆ Ǉ ǈ ǉ Ǌ ǋ ǌ Ǎ ǎ Ǐ
"  U+01D0  ǐ Ǒ ǒ Ǔ ǔ Ǖ ǖ Ǘ ǘ Ǚ ǚ Ǜ ǜ ǝ Ǟ ǟ
"  U+01E0  Ǡ ǡ Ǣ ǣ Ǥ ǥ Ǧ ǧ Ǩ ǩ Ǫ ǫ Ǭ ǭ Ǯ ǯ
"  U+01F0  ǰ Ǳ ǲ ǳ Ǵ ǵ Ƕ Ƿ Ǹ ǹ Ǻ ǻ Ǽ ǽ Ǿ ǿ
"  U+0200  Ȁ ȁ Ȃ ȃ Ȅ ȅ Ȇ ȇ Ȉ ȉ Ȋ ȋ Ȍ ȍ Ȏ ȏ
"  U+0210  Ȑ ȑ Ȓ ȓ Ȕ ȕ Ȗ ȗ Ș ș Ț ț Ȝ ȝ Ȟ ȟ
"  U+0220  Ƞ ȡ Ȣ ȣ Ȥ ȥ Ȧ ȧ Ȩ ȩ Ȫ ȫ Ȭ ȭ Ȯ ȯ
"  U+0230  Ȱ ȱ Ȳ ȳ ȴ ȵ ȶ ȷ ȸ ȹ Ⱥ Ȼ ȼ Ƚ Ⱦ ȿ
"  U+0240  ɀ Ɂ ɂ Ƀ Ʉ Ʌ Ɇ ɇ Ɉ ɉ Ɋ ɋ Ɍ ɍ Ɏ ɏ

"       ▼  IPA Extensions
"  U+0250  ɐ ɑ ɒ ɓ ɔ ɕ ɖ ɗ ɘ ə ɚ ɛ ɜ ɝ ɞ ɟ
"  U+0260  ɠ ɡ ɢ ɣ ɤ ɥ ɦ ɧ ɨ ɩ ɪ ɫ ɬ ɭ ɮ ɯ
"  U+0270  ɰ ɱ ɲ ɳ ɴ ɵ ɶ ɷ ɸ ɹ ɺ ɻ ɼ ɽ ɾ ɿ
"  U+0280  ʀ ʁ ʂ ʃ ʄ ʅ ʆ ʇ ʈ ʉ ʊ ʋ ʌ ʍ ʎ ʏ
"  U+0290  ʐ ʑ ʒ ʓ ʔ ʕ ʖ ʗ ʘ ʙ ʚ ʛ ʜ ʝ ʞ ʟ
"  U+02A0  ʠ ʡ ʢ ʣ ʤ ʥ ʦ ʧ ʨ ʩ ʪ ʫ ʬ ʭ ʮ ʯ

"       ▼  Spacing Modifier Letters
"  U+02B0  ʰ ʱ ʲ ʳ ʴ ʵ ʶ ʷ ʸ ʹ ʺ ʻ ʼ ʽ ʾ ʿ
"  U+02C0  ˀ ˁ ˂ ˃ ˄ ˅ ˆ ˇ ˈ ˉ ˊ ˋ ˌ ˍ ˎ ˏ
"  U+02D0  ː ˑ ˒ ˓ ˔ ˕ ˖ ˗ ˘ ˙ ˚ ˛ ˜ ˝ ˞ ˟
"  U+02E0  ˠ ˡ ˢ ˣ ˤ ˥ ˦ ˧ ˨ ˩ ˪ ˫ ˬ ˭ ˮ ˯
"  U+02F0  ˰ ˱ ˲ ˳ ˴ ˵ ˶ ˷ ˸ ˹ ˺ ˻ ˼ ˽ ˾ ˿

"       ▼  Greek and Coptic
"  U+0380      ΄ ΅ Ά · Έ Ή Ί  Ό  Ύ Ώ

"  U+0390  ΐ Α Β Γ Δ Ε Ζ Η Θ Ι Κ Λ Μ Ν Ξ Ο

ino <C-u>Gamma Γ
ino <C-u>Delta Δ
ino <C-u>Theta Θ
ino <C-u>Lambda Λ
ino <C-u>Xi Ξ

"  U+03A0  Π Ρ   Σ Τ Υ Φ Χ Ψ Ω Ϊ Ϋ ά έ ή ί

ino <C-u>Pi Π
ino <C-u>Sigma Σ
ino <C-u>Upsilon Υ
ino <C-u>Phi Φ
ino <C-u>Psi Ψ
ino <C-u>Omega Ω

"  U+03B0  ΰ α β γ δ ε ζ η θ ι κ λ μ ν ξ ο

ino <C-u>alpha α
ino <C-u>beta β
ino <C-u>gamma γ
ino <C-u>delta δ
ino <C-u>epsilon ε
ino <C-u>zeta ζ
ino <C-u>eta η
ino <C-u>theta θ
ino <C-u>iota ι
ino <C-u>kappa κ
ino <C-u>lambda λ
ino <C-u>mu μ
ino <C-u>nu ν
ino <C-u>xi ξ

"  U+03C0  π ρ ς σ τ υ φ χ ψ ω ϊ ϋ ό ύ ώ

ino <C-u>pi π
ino <C-u>rho ρ
ino <C-u>sigma σ
ino <C-u>tau τ
ino <C-u>upsilon υ
ino <C-u>phi φ
ino <C-u>chi χ
ino <C-u>psi ψ
ino <C-u>omega ω

"  U+03D0  ϐ ϑ   ϕ ϖ               Ϝ ϝ
"  U+03F0  ϰ ϱ   ϵ


"       ▼  Phonetic extensions
"  U+1D00  ᴀ ᴁ ᴂ ᴃ ᴄ ᴅ ᴆ ᴇ ᴈ ᴉ ᴊ ᴋ ᴌ ᴍ ᴎ ᴏ
"  U+1D10  ᴐ ᴑ ᴒ ᴓ ᴔ ᴕ ᴖ ᴗ ᴘ ᴙ ᴚ ᴛ ᴜ ᴝ ᴞ ᴟ
"  U+1D20  ᴠ ᴡ ᴢ ᴣ ᴤ ᴥ ᴦ ᴧ ᴨ ᴩ ᴪ ᴫ ᴬ ᴭ ᴮ ᴯ

ino <C-u>^A ᴬ
ino <C-u>^B ᴮ

"  U+1D30  ᴰ ᴱ ᴲ ᴳ ᴴ ᴵ ᴶ ᴷ ᴸ ᴹ ᴺ ᴻ ᴼ ᴽ ᴾ ᴿ

ino <C-u>^D ᴰ
ino <C-u>^E ᴱ
ino <C-u>^G ᴳ
ino <C-u>^H ᴴ
ino <C-u>^I ᴵ
ino <C-u>^J ᴶ
ino <C-u>^K ᴷ
ino <C-u>^L ᴸ
ino <C-u>^M ᴹ
ino <C-u>^N ᴺ
ino <C-u>^O ᴼ
ino <C-u>^P ᴾ
ino <C-u>^R ᴿ

"  U+1D40  ᵀ ᵁ ᵂ ᵃ ᵄ ᵅ ᵆ ᵇ ᵈ ᵉ ᵊ ᵋ ᵌ ᵍ ᵎ ᵏ

ino <C-u>^T ᵀ
ino <C-u>^U ᵁ
ino <C-u>^W ᵂ
ino <C-u>^a ᵃ
ino <C-u>^alpha ᵅ
ino <C-u>^b ᵇ
ino <C-u>^d ᵈ
ino <C-u>^e ᵉ
ino <C-u>^g ᵍ
ino <C-u>^k ᵏ

"  U+1D50  ᵐ ᵑ ᵒ ᵓ ᵔ ᵕ ᵖ ᵗ ᵘ ᵙ ᵚ ᵛ ᵜ ᵝ ᵞ ᵟ

ino <C-u>^m ᵐ
ino <C-u>^o ᵒ
ino <C-u>^p ᵖ
ino <C-u>^t ᵗ
ino <C-u>^u ᵘ
ino <C-u>^v ᵛ
ino <C-u>^beta ᵝ
ino <C-u>^gamma ᵞ
ino <C-u>^delta ᵟ

"  U+1D60  ᵠ ᵡ ᵢ ᵣ ᵤ ᵥ ᵦ ᵧ ᵨ ᵩ ᵪ ᵫ ᵬ ᵭ ᵮ ᵯ

ino <C-u>^phi ᵠ
ino <C-u>^chi ᵡ
ino <C-u>_i ᵢ
ino <C-u>_r ᵣ
ino <C-u>_u ᵤ
ino <C-u>_v ᵥ
ino <C-u>_beta ᵦ
ino <C-u>_gamma ᵧ
ino <C-u>_rho ᵨ
ino <C-u>_phi ᵩ
ino <C-u>_chi ᵪ

"  U+1D70  ᵰ ᵱ ᵲ ᵳ ᵴ ᵵ ᵶ ᵷ ᵸ ᵹ ᵺ ᵻ ᵼ ᵽ ᵾ ᵿ

"       ▼  General Punctuation
"  U+2000
"  U+2010  ‐ ‑ ‒ – — ― ‖ ‗ ‘ ’ ‚ ‛ “ ” „ ‟
"  U+2020  † ‡ • ‣ ․ ‥ … ‧

"  U+2030  ‰ ‱ ′ ″ ‴ ‵ ‶ ‷ ‸ ‹ › ※ ‼ ‽ ‾ ‿

ino <C-u>!! ‼

"  U+2040  ⁀ ⁁ ⁂ ⁃ ⁄ ⁅ ⁆ ⁇ ⁈ ⁉ ⁊ ⁋ ⁌ ⁍ ⁎ ⁏

ino <C-u>?? ⁇
ino <C-u>?! ⁈
ino <C-u>!? ⁉
ino <C-u>_* ⁎

"  U+2050  ⁐ ⁑ ⁒ ⁓ ⁔ ⁕ ⁖ ⁗ ⁘ ⁙ ⁚ ⁛ ⁜ ⁝ ⁞
"  U+2060

"       ▼  Superscripts and Subscripts
"  U+2070  ⁰ ⁱ   ⁴ ⁵ ⁶ ⁷ ⁸ ⁹ ⁺ ⁻ ⁼ ⁽ ⁾ ⁿ

ino <C-u>^0 ⁰
ino <C-u>^i ⁱ
ino <C-u>^4 ⁴
ino <C-u>^5 ⁵
ino <C-u>^6 ⁶
ino <C-u>^7 ⁷
ino <C-u>^8 ⁸
ino <C-u>^9 ⁹
ino <C-u>^+ ⁺
ino <C-u>^- ⁻
ino <C-u>^= ⁼
ino <C-u>^( ⁽
ino <C-u>^) ⁾
ino <C-u>^n ⁿ

"  U+2080  ₀ ₁ ₂ ₃ ₄ ₅ ₆ ₇ ₈ ₉ ₊ ₋ ₌ ₍ ₎

ino <C-u>_0 ₀
ino <C-u>_1 ₁
ino <C-u>_2 ₂
ino <C-u>_3 ₃
ino <C-u>_4 ₄
ino <C-u>_5 ₅
ino <C-u>_6 ₆
ino <C-u>_7 ₇
ino <C-u>_8 ₈
ino <C-u>_9 ₉
ino <C-u>_+ ₊
ino <C-u>_- ₋
ino <C-u>_= ₌
ino <C-u>_( ₍
ino <C-u>_) ₎

"  U+2090  ₐ ₑ ₒ ₓ ₔ ₕ ₖ ₗ ₘ ₙ ₚ ₛ ₜ

ino <C-u>_a ₐ
ino <C-u>_e ₑ
ino <C-u>_o ₒ
ino <C-u>_x ₓ
ino <C-u>_h ₕ
ino <C-u>_k ₖ
ino <C-u>_l ₗ
ino <C-u>_m ₘ
ino <C-u>_n ₙ
ino <C-u>_p ₚ
ino <C-u>_s ₛ
ino <C-u>_t ₜ

"       ▼  Currency Symbols
"  U+20A0            ₪ ₫ €
"  U+20B0        ₽

"       ▼  Letterlike Symbols
"  U+2100  ℀ ℁ ℂ ℃ ℄ ℅ ℆ ℇ ℈ ℉ ℊ ℋ ℌ ℍ ℎ ℏ

ino <C-U>euler ℇ

"  U+2110  ℐ ℑ ℒ ℓ ℔ ℕ № ℗ ℘ ℙ ℚ ℛ ℜ ℝ ℞ ℟

"  U+2120  ℠ ℡ ™ ℣ ℤ ℥ Ω ℧ ℨ ℩ K Å ℬ ℭ ℮ ℯ

ino <C-U>tm ™

"  U+2130  ℰ ℱ Ⅎ ℳ ℴ ℵ ℶ ℷ ℸ ℹ ℺ ℻ ℼ ℽ ℾ ℿ

ino <C-U>info ℹ
ino <C-U>[pi ℼ
ino <C-U>[gamma ℽ
ino <C-U>[Gamma ℾ
ino <C-U>[Pi ℿ

"  U+2140  ⅀ ⅁ ⅂ ⅃ ⅄ ⅅ ⅆ ⅇ ⅈ ⅉ ⅊ ⅋ ⅌ ⅍ ⅎ ⅏

ino <C-U>[nsumm ⅀
ino <C-U>[/D ⅅ
ino <C-U>[/d ⅆ
ino <C-U>[/e ⅇ
ino <C-U>[/i ⅈ
ino <C-U>[/j ⅉ

"       ▼  Number Forms
"  U+2150  ⅐ ⅑ ⅒ ⅓ ⅔ ⅕ ⅖ ⅗ ⅘ ⅙ ⅚ ⅛ ⅜ ⅝ ⅞ ⅟

ino <C-U>1/7 ⅐
ino <C-U>1/9 ⅑
ino <C-U>1/10 ⅒
ino <C-U>1/3 ⅓
ino <C-U>2/3 ⅔
ino <C-U>1/5 ⅕
ino <C-U>2/5 ⅖
ino <C-U>3/5 ⅗
ino <C-U>4/5 ⅘
ino <C-U>1/6 ⅙
ino <C-U>5/6 ⅚
ino <C-U>1/8 ⅛
ino <C-U>3/8 ⅜
ino <C-U>5/8 ⅝
ino <C-U>7/8 ⅞

"  U+2160  Ⅰ Ⅱ Ⅲ Ⅳ Ⅴ Ⅵ Ⅶ Ⅷ Ⅸ Ⅹ Ⅺ Ⅻ Ⅼ Ⅽ Ⅾ Ⅿ

ino <C-U>romanI Ⅰ
ino <C-U>romanII Ⅱ
ino <C-U>romanIII Ⅲ
ino <C-U>romanIV Ⅳ
ino <C-U>romanV Ⅴ
ino <C-U>romanVI Ⅵ
ino <C-U>romanVII Ⅶ
ino <C-U>romanVIII Ⅷ
ino <C-U>romanIX Ⅸ
ino <C-U>romanX Ⅹ
ino <C-U>romanXI Ⅺ
ino <C-U>romanXII Ⅻ
ino <C-U>romanL Ⅼ
ino <C-U>romanC Ⅽ
ino <C-U>romanD Ⅾ
ino <C-U>romanM Ⅿ

"  U+2170  ⅰ ⅱ ⅲ ⅳ ⅴ ⅵ ⅶ ⅷ ⅸ ⅹ ⅺ ⅻ ⅼ ⅽ ⅾ ⅿ

ino <C-U>romani ⅰ
ino <C-U>romanii ⅱ
ino <C-U>romaniii ⅲ
ino <C-U>romaniv ⅳ
ino <C-U>romanv ⅴ
ino <C-U>romanvi ⅵ
ino <C-U>romanvii ⅶ
ino <C-U>romanviii ⅷ
ino <C-U>romanix ⅸ
ino <C-U>romanx ⅹ
ino <C-U>romanxi ⅺ
ino <C-U>romanxii ⅻ
ino <C-U>romanl ⅼ
ino <C-U>romanc ⅽ
ino <C-U>romand ⅾ
ino <C-U>romanm ⅿ

"  U+2180  ↀ ↁ ↂ Ↄ ↄ ↅ ↆ ↇ ↈ ↉ ↊ ↋


"       ▼  Arrows
"  U+2190  ← ↑ → ↓ ↔ ↕ ↖ ↗ ↘ ↙ ↚ ↛ ↜ ↝ ↞ ↟

ino <C-U><- ←
ino <C-U>-^ ↑
ino <C-U>-> →
ino <C-U>-v ↓
ino <C-U><~ ↜
ino <C-U>~> ↝
ino <C-U><<- ↞
ino <C-U>-^^ ↟

"  U+21A0  ↠ ↡ ↢ ↣ ↤ ↥ ↦ ↧ ↨ ↩ ↪ ↫ ↬ ↭ ↮ ↯

ino <C-U>->> ↠
ino <C-U>-vv ↡
ino <C-U><-( ↢
ino <C-U>)-> ↣
ino <C-U><-<Bar> ↤
ino <C-U><Bar>-^ ↥
ino <C-U><Bar>-> ↦
ino <C-U><Bar>-v ↧
ino <C-U></ ↩
ino <C-U>\> ↪

"  U+21B0  ↰ ↱ ↲ ↳ ↴ ↵ ↶ ↷ ↸ ↹ ↺ ↻ ↼ ↽ ↾ ↿
"  U+21C0  ⇀ ⇁ ⇂ ⇃ ⇄ ⇅ ⇆ ⇇ ⇈ ⇉ ⇊ ⇋ ⇌ ⇍ ⇎ ⇏

"  U+21D0  ⇐ ⇑ ⇒ ⇓ ⇔ ⇕ ⇖ ⇗ ⇘ ⇙ ⇚ ⇛ ⇜ ⇝ ⇞ ⇟
ino <C-U><= ⇐
ino <C-U>=^ ⇑
ino <C-U>=> ⇒
ino <C-U>=v ⇓
ino <C-U><=> ⇔
ino <C-U>^=v ⇕

"  U+21E0  ⇠ ⇡ ⇢ ⇣ ⇤ ⇥ ⇦ ⇧ ⇨ ⇩ ⇪ ⇫ ⇬ ⇭ ⇮ ⇯
"  U+21F0  ⇰ ⇱ ⇲ ⇳ ⇴ ⇵ ⇶ ⇷ ⇸ ⇹ ⇺ ⇻ ⇼ ⇽ ⇾ ⇿

"       ▼  Mathematical Operators
"  U+2200  ∀ ∁ ∂ ∃ ∄ ∅ ∆ ∇ ∈ ∉ ∊ ∋ ∌ ∍ ∎ ∏

ino <C-u>forall ∀
ino <C-u>complement ∁
ino <C-u>pdiff ∂
ino <C-u>exists ∃
ino <C-u>nexists ∄
ino <C-u>empty ∅
ino <C-u>incr ∆
ino <C-u>nabla ∇
ino <C-u>elem ∈
ino <C-u>nelem ∉
ino <C-u>selem ∊
ino <C-u>contains ∋
ino <C-u>ncontains ∌
ino <C-u>scontains ∍
ino <C-u>endofproof ∎
ino <C-u>nproduct ∏

"  U+2210  ∐ ∑ − ∓ ∔ ∕ ∖ ∗ ∘ ∙ √ ∛ ∜ ∝ ∞ ∟

ino <C-u>ncoproduct ∐
ino <C-u>nsum ∑
ino <C-u>minus −
ino <C-u>minusplus ∓
ino <C-u>dotplus ∔
ino <C-u>divslash ∕
ino <C-u>setminus ∖
ino <C-u>asterisk ∗
ino <C-u>ring ∘
ino <C-u>bullet ∙
ino <C-u>root2 √
ino <C-u>root3 ∛
ino <C-u>root4 ∜
ino <C-u>proportional ∝
ino <C-u>infinity ∞
ino <C-u>rangle ∟

"  U+2220  ∠ ∡ ∢ ∣ ∤ ∥ ∦ ∧ ∨ ∩ ∪ ∫ ∬ ∭ ∮ ∯

ino <C-u>angle ∠
ino <C-u>mangle ∡
ino <C-u>sangle ∢
ino <C-u>divides ∣
ino <C-u>ndivides ∤
ino <C-u>parallel ∥
ino <C-u>nparallel ∦
ino <C-u>and ∧
ino <C-u>or ∨
ino <C-u>intersection ∩
ino <C-u>union ∪
ino <C-u>integral ∫
ino <C-u>integral2 ∬
ino <C-u>integral3 ∭

"  U+2230  ∰ ∱ ∲ ∳ ∴ ∵ ∶ ∷ ∸ ∹ ∺ ∻ ∼ ∽ ∾ ∿

ino <C-u>therefore ∴
ino <C-u>because ∵
ino <C-u>ratio ∶
ino <C-u>proportion ∷
ino <C-u>:: ∷
ino <C-u>dotminus ∸
ino <C-u>excess ∹
ino <C-u>gproportion ∺
ino <C-u>homothetic ∻
ino <C-u>~ ∼
ino <C-U>rtilde ∽
ino <C-U>sine ∿

"  U+2240  ≀ ≁ ≂ ≃ ≄ ≅ ≆ ≇ ≈ ≉ ≊ ≋ ≌ ≍ ≎ ≏

ino <C-U>wreath ≀
ino <C-U>/~ ≁
ino <C-U>-~ ≂
ino <C-U>~- ≃
ino <C-U>/~- ≄
ino <C-U>~= ≅
ino <C-U>~/= ≆
ino <C-U>/~= ≇
ino <C-U>~~ ≈
ino <C-U>/~~ ≉
ino <C-U>~~- ≊
ino <C-U>~~~ ≋

"  U+2250  ≐ ≑ ≒ ≓ ≔ ≕ ≖ ≗ ≘ ≙ ≚ ≛ ≜ ≝ ≞ ≟

ino <C-U>:= ≔
ino <C-U>=: ≕
ino <C-U>def= ≝
ino <C-U>?= ≟

"  U+2260  ≠ ≡ ≢ ≣ ≤ ≥ ≦ ≧ ≨ ≩ ≪ ≫ ≬ ≭ ≮ ≯

ino <C-U>/= ≠
ino <C-U>=3 ≡
ino <C-U>/=3 ≢
ino <C-U>=4 ≣
ino <C-U>=< ≤
ino <C-U>>= ≥
ino <C-U><< ≪
ino <C-U>>> ≫
ino <C-U>/< ≮
ino <C-U>/> ≯

"  U+2270  ≰ ≱ ≲ ≳ ≴ ≵ ≶ ≷ ≸ ≹ ≺ ≻ ≼ ≽ ≾ ≿

ino <C-U>/=< ≰
ino <C-U>/>= ≱

"  U+2280  ⊀ ⊁ ⊂ ⊃ ⊄ ⊅ ⊆ ⊇ ⊈ ⊉ ⊊ ⊋ ⊌ ⊍ ⊎ ⊏

ino <C-U>psubset ⊂
ino <C-U>psuperset ⊃
ino <C-U>/psubset ⊄
ino <C-U>/psuperset ⊅
ino <C-U>subset ⊆
ino <C-U>superset ⊇
ino <C-U>/subset ⊈
ino <C-U>/superset ⊉
ino <C-U>multiset ⊌
ino <C-U>multisetmult ⊍
ino <C-U>multisetunion ⊎

"  U+2290  ⊐ ⊑ ⊒ ⊓ ⊔ ⊕ ⊖ ⊗ ⊘ ⊙ ⊚ ⊛ ⊜ ⊝ ⊞ ⊟

ino <C-U>o+ ⊕
ino <C-U>o- ⊖
ino <C-U>ox ⊗
ino <C-U>o/ ⊘
ino <C-U>o. ⊙
ino <C-U>oo ⊚
ino <C-U>o* ⊛
ino <C-U>o= ⊜
ino <C-U>s+ ⊞
ino <C-U>s- ⊟

"  U+22A0  ⊠ ⊡ ⊢ ⊣ ⊤ ⊥ ⊦ ⊧ ⊨ ⊩ ⊪ ⊫ ⊬ ⊭ ⊮ ⊯

ino <C-U>sx ⊠
ino <C-U>s. ⊡
ino <C-U>top ⊤
ino <C-U>bottom ⊥

"  U+22B0  ⊰ ⊱ ⊲ ⊳ ⊴ ⊵ ⊶ ⊷ ⊸ ⊹ ⊺ ⊻ ⊼ ⊽ ⊾ ⊿

ino <C-U>-o ⊸
ino <C-U>xor ⊻
ino <C-U>nand ⊼
ino <C-U>nor ⊽

"  U+22C0  ⋀ ⋁ ⋂ ⋃ ⋄ ⋅ ⋆ ⋇ ⋈ ⋉ ⋊ ⋋ ⋌ ⋍ ⋎ ⋏

ino <C-U>n-and ⋀
ino <C-U>n-or ⋁
ino <C-U>n-intersect ⋂
ino <C-U>n-union ⋃
ino <C-U>diamond ⋄
ino <C-U>dot ⋅
ino <C-U>star ⋆
ino <C-U>bowtie ⋈
ino <C-U>cor ⋎
ino <C-U>cand ⋏

"  U+22D0  ⋐ ⋑ ⋒ ⋓ ⋔ ⋕ ⋖ ⋗ ⋘ ⋙ ⋚ ⋛ ⋜ ⋝ ⋞ ⋟

ino <C-U><<< ⋘
ino <C-U>>>> ⋙

"  U+22E0  ⋠ ⋡ ⋢ ⋣ ⋤ ⋥ ⋦ ⋧ ⋨ ⋩ ⋪ ⋫ ⋬ ⋭ ⋮ ⋯
"  U+22F0  ⋰ ⋱ ⋲ ⋳ ⋴ ⋵ ⋶ ⋷ ⋸ ⋹ ⋺ ⋻ ⋼ ⋽ ⋾ ⋿

"       ▼  Miscellaneous Technical
"  U+2300  ⌀ ⌁ ⌂ ⌃ ⌄ ⌅ ⌆ ⌇ ⌈ ⌉ ⌊ ⌋ ⌌ ⌍ ⌎ ⌏
"  U+2310  ⌐ ⌑ ⌒ ⌓ ⌔ ⌕ ⌖ ⌗ ⌘ ⌙ ⌚ ⌛ ⌜ ⌝ ⌞ ⌟
"  U+2320  ⌠ ⌡ ⌢ ⌣ ⌤ ⌥ ⌦ ⌧ ⌨ 〈 〉 ⌫ ⌬
"  U+2330      ⌴  ⌶ ⌷ ⌸ ⌹ ⌺ ⌻ ⌼ ⌽ ⌾ ⌿
"  U+2340  ⍀ ⍁ ⍂ ⍃ ⍄ ⍅ ⍆ ⍇ ⍈ ⍉ ⍊ ⍋ ⍌ ⍍ ⍎ ⍏
"  U+2350  ⍐ ⍑ ⍒ ⍓ ⍔ ⍕ ⍖ ⍗ ⍘ ⍙ ⍚ ⍛ ⍜ ⍝ ⍞ ⍟
"  U+2360  ⍠ ⍡ ⍢ ⍣ ⍤ ⍥ ⍦ ⍧ ⍨ ⍩ ⍪ ⍫ ⍬ ⍭ ⍮ ⍯
"  U+2370  ⍰ ⍱ ⍲ ⍳ ⍴ ⍵ ⍶ ⍷ ⍸ ⍹ ⍺   ⍽
"  U+2380  ⎀ ⎁ ⎂ ⎃ ⎄ ⎅ ⎆ ⎇ ⎈ ⎉ ⎊ ⎋ ⎌ ⎍ ⎎ ⎏
"  U+2390  ⎐ ⎑ ⎒ ⎓ ⎔ ⎕ ⎖ ⎗ ⎘ ⎙ ⎚ ⎛ ⎜ ⎝ ⎞ ⎟
"  U+23A0  ⎠ ⎡ ⎢ ⎣ ⎤ ⎥ ⎦ ⎧ ⎨ ⎩ ⎪ ⎫ ⎬ ⎭ ⎮ ⎯
"  U+23B0  ⎰ ⎱ ⎲ ⎳ ⎴ ⎵ ⎶ ⎷ ⎸ ⎹ ⎺ ⎻ ⎼ ⎽
"  U+23C0                ⏏
"  U+23D0  ⏐                       ⏜ ⏝ ⏞ ⏟
"  U+23E0  ⏠ ⏡
"  U+23F0        ⏳             ⏻ ⏼ ⏽ ⏾

"       ▼  Enclosed Alphanumerics
"  U+2460  ① ② ③ ④ ⑤ ⑥ ⑦ ⑧ ⑨
"  U+24B0              Ⓐ Ⓑ Ⓒ Ⓓ Ⓔ Ⓕ Ⓖ Ⓗ Ⓘ Ⓙ
"  U+24C0  Ⓚ Ⓛ Ⓜ Ⓝ Ⓞ Ⓟ Ⓠ Ⓡ Ⓢ Ⓣ Ⓤ Ⓥ Ⓦ Ⓧ Ⓨ Ⓩ
"  U+24D0  ⓐ ⓑ ⓒ ⓓ ⓔ ⓕ ⓖ ⓗ ⓘ ⓙ ⓚ ⓛ ⓜ ⓝ ⓞ ⓟ
"  U+24E0  ⓠ ⓡ ⓢ ⓣ ⓤ ⓥ ⓦ ⓧ ⓨ ⓩ ⓪

"       ▼  Misc. Mathematical Symbols-A
"  U+27C0  ⟀ ⟁ ⟂ ⟃ ⟄ ⟅ ⟆ ⟇ ⟈ ⟉ ⟊   ⟌
"  U+27D0  ⟐ ⟑ ⟒ ⟓ ⟔ ⟕ ⟖ ⟗ ⟘ ⟙ ⟚ ⟛ ⟜ ⟝ ⟞ ⟟
"  U+27E0  ⟠ ⟡ ⟢ ⟣ ⟤ ⟥ ⟦ ⟧ ⟨ ⟩ ⟪ ⟫ ⟬ ⟭ ⟮ ⟯

ino <C-U>[[ ⟦
ino <C-U>]] ⟧
ino <C-U>[< ⟨
ino <C-U>>] ⟩
ino <C-U>[<< ⟪
ino <C-U>>>] ⟫

"       ▼  Suppl. Arrows-A
"  U+27F0  ⟰ ⟱ ⟲ ⟳ ⟴ ⟵ ⟶ ⟷ ⟸ ⟹ ⟺ ⟻ ⟼ ⟽ ⟾ ⟿

"       ▼  Supplemental Arrows-B
"  U+2900  ⤀ ⤁ ⤂ ⤃ ⤄ ⤅ ⤆ ⤇ ⤈ ⤉ ⤊ ⤋ ⤌ ⤍ ⤎ ⤏
"  U+2910  ⤐ ⤑ ⤒ ⤓ ⤔ ⤕ ⤖ ⤗ ⤘ ⤙ ⤚ ⤛ ⤜ ⤝ ⤞ ⤟
"  U+2920  ⤠ ⤡ ⤢ ⤣ ⤤ ⤥ ⤦ ⤧ ⤨ ⤩ ⤪ ⤫ ⤬ ⤭ ⤮ ⤯
"  U+2930  ⤰ ⤱ ⤲ ⤳ ⤴ ⤵ ⤶ ⤷ ⤸ ⤹ ⤺ ⤻ ⤼ ⤽ ⤾ ⤿
"  U+2940  ⥀ ⥁ ⥂ ⥃ ⥄ ⥅ ⥆ ⥇ ⥈ ⥉ ⥊ ⥋ ⥌ ⥍ ⥎ ⥏
"  U+2950  ⥐ ⥑ ⥒ ⥓ ⥔ ⥕ ⥖ ⥗ ⥘ ⥙ ⥚ ⥛ ⥜ ⥝ ⥞ ⥟
"  U+2960  ⥠ ⥡ ⥢ ⥣ ⥤ ⥥ ⥦ ⥧ ⥨ ⥩ ⥪ ⥫ ⥬ ⥭ ⥮ ⥯
"  U+2970  ⥰ ⥱ ⥲ ⥳ ⥴ ⥵ ⥶ ⥷ ⥸ ⥹ ⥺ ⥻ ⥼ ⥽ ⥾ ⥿

"       ▼  Misc. Math Symbols-B
"  U+2980  ⦀ ⦁ ⦂ ⦃ ⦄ ⦅ ⦆ ⦇ ⦈ ⦉ ⦊ ⦋ ⦌ ⦍ ⦎ ⦏
"  U+2990  ⦐ ⦑ ⦒ ⦓ ⦔ ⦕ ⦖ ⦗ ⦘ ⦙ ⦚ ⦛ ⦜ ⦝ ⦞ ⦟
"  U+29A0  ⦠ ⦡ ⦢ ⦣ ⦤ ⦥ ⦦ ⦧ ⦨ ⦩ ⦪ ⦫ ⦬ ⦭ ⦮ ⦯
"  U+29B0  ⦰ ⦱ ⦲ ⦳ ⦴ ⦵ ⦶ ⦷ ⦸ ⦹ ⦺ ⦻ ⦼ ⦽ ⦾ ⦿
"  U+29C0  ⧀ ⧁ ⧂ ⧃ ⧄ ⧅ ⧆ ⧇ ⧈ ⧉ ⧊ ⧋ ⧌ ⧍ ⧎ ⧏
"  U+29D0  ⧐ ⧑ ⧒ ⧓ ⧔ ⧕ ⧖ ⧗ ⧘ ⧙ ⧚ ⧛ ⧜ ⧝ ⧞ ⧟
"  U+29E0  ⧠ ⧡ ⧢ ⧣ ⧤ ⧥ ⧦ ⧧ ⧨ ⧩ ⧪ ⧫ ⧬ ⧭ ⧮ ⧯
"  U+29F0  ⧰ ⧱ ⧲ ⧳ ⧴ ⧵ ⧶ ⧷ ⧸ ⧹ ⧺ ⧻ ⧼ ⧽ ⧾ ⧿

ino <C-U>spot ⦁
ino <C-U>: ⦂
ino <C-U>{{ ⦃
ino <C-U>}} ⦄
ino <C-U>(( ⦅
ino <C-U>)) ⦆

"       ▼  Supplemental Math Operators
"  U+2A00  ⨀ ⨁ ⨂ ⨃ ⨄ ⨅ ⨆ ⨇ ⨈ ⨉ ⨊ ⨋ ⨌ ⨍ ⨎ ⨏
"  U+2A10  ⨐ ⨑ ⨒ ⨓ ⨔ ⨕ ⨖ ⨗ ⨘ ⨙ ⨚ ⨛ ⨜ ⨝ ⨞ ⨟
"  U+2A20  ⨠ ⨡ ⨢ ⨣ ⨤ ⨥ ⨦ ⨧ ⨨ ⨩ ⨪ ⨫ ⨬ ⨭ ⨮ ⨯
"  U+2A30  ⨰ ⨱ ⨲ ⨳ ⨴ ⨵ ⨶ ⨷ ⨸ ⨹ ⨺ ⨻ ⨼ ⨽ ⨾ ⨿
"  U+2A40  ⩀ ⩁ ⩂ ⩃ ⩄ ⩅ ⩆ ⩇ ⩈ ⩉ ⩊ ⩋ ⩌ ⩍ ⩎ ⩏
"  U+2A50  ⩐ ⩑ ⩒ ⩓ ⩔ ⩕ ⩖ ⩗ ⩘ ⩙ ⩚ ⩛ ⩜ ⩝ ⩞ ⩟
"  U+2A60  ⩠ ⩡ ⩢ ⩣ ⩤ ⩥ ⩦ ⩧ ⩨ ⩩ ⩪ ⩫ ⩬ ⩭ ⩮ ⩯
"  U+2A70  ⩰ ⩱ ⩲ ⩳ ⩴ ⩵ ⩶ ⩷ ⩸ ⩹ ⩺ ⩻ ⩼ ⩽ ⩾ ⩿
"  U+2A80  ⪀ ⪁ ⪂ ⪃ ⪄ ⪅ ⪆ ⪇ ⪈ ⪉ ⪊ ⪋ ⪌ ⪍ ⪎ ⪏
"  U+2A90  ⪐ ⪑ ⪒ ⪓ ⪔ ⪕ ⪖ ⪗ ⪘ ⪙ ⪚ ⪛ ⪜ ⪝ ⪞ ⪟
"  U+2AA0  ⪠ ⪡ ⪢ ⪣ ⪤ ⪥ ⪦ ⪧ ⪨ ⪩ ⪪ ⪫ ⪬ ⪭ ⪮ ⪯
"  U+2AB0  ⪰ ⪱ ⪲ ⪳ ⪴ ⪵ ⪶ ⪷ ⪸ ⪹ ⪺ ⪻ ⪼ ⪽ ⪾ ⪿
"  U+2AC0  ⫀ ⫁ ⫂ ⫃ ⫄ ⫅ ⫆ ⫇ ⫈ ⫉ ⫊ ⫋ ⫌ ⫍ ⫎ ⫏
"  U+2AD0  ⫐ ⫑ ⫒ ⫓ ⫔ ⫕ ⫖ ⫗ ⫘ ⫙ ⫚ ⫛ ⫝̸ ⫝ ⫞ ⫟
"  U+2AE0  ⫠ ⫡ ⫢ ⫣ ⫤ ⫥ ⫦ ⫧ ⫨ ⫩ ⫪ ⫫ ⫬ ⫭ ⫮ ⫯
"  U+2AF0  ⫰ ⫱ ⫲ ⫳ ⫴ ⫵ ⫶ ⫷ ⫸ ⫹ ⫺ ⫻ ⫼ ⫽ ⫾ ⫿

ino <C-u>; ⨾
ino <C-U><Bar><Bar><Bar> ⫴
ino <C-U>/// ⫻

"       ▼  Misc. Symbols and Arrows
"  U+2B00  ⬀ ⬁ ⬂ ⬃ ⬄ ⬅ ⬆ ⬇ ⬈ ⬉ ⬊ ⬋ ⬌ ⬍ ⬎ ⬏
"  U+2B10  ⬐ ⬑ ⬒ ⬓ ⬔ ⬕ ⬖ ⬗ ⬘ ⬙ ⬚ ⬛ ⬜ ⬝ ⬞ ⬟
"  U+2B20  ⬠ ⬡ ⬢ ⬣ ⬤ ⬥ ⬦ ⬧ ⬨ ⬩ ⬪ ⬫ ⬬ ⬭ ⬮ ⬯
"  U+2B30  ⬰ ⬱ ⬲ ⬳ ⬴ ⬵ ⬶ ⬷ ⬸ ⬹ ⬺ ⬻ ⬼ ⬽ ⬾ ⬿
"  U+2B40  ⭀ ⭁ ⭂ ⭃ ⭄ ⭅ ⭆ ⭇ ⭈ ⭉ ⭊ ⭋ ⭌ ⭍ ⭎ ⭏
"  U+2B50  ⭐ ⭑ ⭒ ⭓ ⭔ ⭕ ⭖ ⭗ ⭘ ⭙ ⭚ ⭛ ⭜ ⭝ ⭞ ⭟
"  U+2B60  ⭠ ⭡ ⭢ ⭣ ⭤ ⭥ ⭦ ⭧ ⭨ ⭩ ⭪ ⭫ ⭬ ⭭ ⭮ ⭯
"  U+2B70  ⭰ ⭱ ⭲ ⭳    ⭶ ⭷ ⭸ ⭹ ⭺ ⭻ ⭼ ⭽ ⭾ ⭿
"  U+2B80  ⮀ ⮁ ⮂ ⮃ ⮄ ⮅ ⮆ ⮇ ⮈ ⮉ ⮊ ⮋ ⮌ ⮍ ⮎ ⮏
"  U+2B90  ⮐ ⮑ ⮒ ⮓ ⮔ ⮕    ⮘ ⮙ ⮚ ⮛ ⮜ ⮝ ⮞ ⮟
"  U+2BA0  ⮠ ⮡ ⮢ ⮣ ⮤ ⮥ ⮦ ⮧ ⮨ ⮩ ⮪ ⮫ ⮬ ⮭ ⮮ ⮯
"  U+2BB0  ⮰ ⮱ ⮲ ⮳ ⮴ ⮵ ⮶ ⮷ ⮸ ⮹      ⮽ ⮾ ⮿
"  U+2BC0  ⯀ ⯁ ⯂ ⯃ ⯄ ⯅ ⯆ ⯇ ⯈   ⯊ ⯋ ⯌ ⯍ ⯎ ⯏
"  U+2BD0  ⯐ ⯑
"  U+2BE0                             ⯬ ⯭ ⯮ ⯯
"  U+2C70    ⱱ
"  U+2E20                 ⸮

" See https://en.wikipedia.org/wiki/Mathematical_Alphanumeric_Symbols
" Holes are filled here (e.g. planck constant ℎ is what we use for script h)
"
"       ▼  Mathematical Alphan. Symbols
"
" 𝐀 𝐁 𝐂 𝐃 𝐄 𝐅 𝐆 𝐇 𝐈 𝐉 𝐊 𝐋 𝐌 𝐍 𝐎 𝐏 𝐐 𝐑 𝐒 𝐓 𝐔 𝐕 𝐖 𝐗 𝐘 𝐙
" 𝐚 𝐛 𝐜 𝐝 𝐞 𝐟 𝐠 𝐡 𝐢 𝐣 𝐤 𝐥 𝐦 𝐧 𝐨 𝐩 𝐪 𝐫 𝐬 𝐭 𝐮 𝐯 𝐰 𝐱 𝐲 𝐳

ino <C-U>bA 𝐀
ino <C-U>bB 𝐁
ino <C-U>bC 𝐂
ino <C-U>bD 𝐃
ino <C-U>bE 𝐄
ino <C-U>bF 𝐅
ino <C-U>bG 𝐆
ino <C-U>bH 𝐇
ino <C-U>bI 𝐈
ino <C-U>bJ 𝐉
ino <C-U>bK 𝐊
ino <C-U>bL 𝐋
ino <C-U>bM 𝐌
ino <C-U>bN 𝐍
ino <C-U>bO 𝐎
ino <C-U>bP 𝐏
ino <C-U>bQ 𝐐
ino <C-U>bR 𝐑
ino <C-U>bS 𝐒
ino <C-U>bT 𝐓
ino <C-U>bU 𝐔
ino <C-U>bV 𝐕
ino <C-U>bW 𝐖
ino <C-U>bX 𝐗
ino <C-U>bY 𝐘
ino <C-U>bZ 𝐙
ino <C-U>ba 𝐚
ino <C-U>bb 𝐛
ino <C-U>bc 𝐜
ino <C-U>bd 𝐝
ino <C-U>be 𝐞
ino <C-U>bf 𝐟
ino <C-U>bg 𝐠
ino <C-U>bh 𝐡
ino <C-U>bi 𝐢
ino <C-U>bj 𝐣
ino <C-U>bk 𝐤
ino <C-U>bl 𝐥
ino <C-U>bm 𝐦
ino <C-U>bn 𝐧
ino <C-U>bo 𝐨
ino <C-U>bp 𝐩
ino <C-U>bq 𝐪
ino <C-U>br 𝐫
ino <C-U>bs 𝐬
ino <C-U>bt 𝐭
ino <C-U>bu 𝐮
ino <C-U>bv 𝐯
ino <C-U>bw 𝐰
ino <C-U>bx 𝐱
ino <C-U>by 𝐲
ino <C-U>bz 𝐳

" 𝐴 𝐵 𝐶 𝐷 𝐸 𝐹 𝐺 𝐻 𝐼 𝐽 𝐾 𝐿 𝑀 𝑁 𝑂 𝑃 𝑄 𝑅 𝑆 𝑇 𝑈 𝑉 𝑊 𝑋 𝑌 𝑍
" 𝑎 𝑏 𝑐 𝑑 𝑒 𝑓 𝑔   𝑖 𝑗 𝑘 𝑙 𝑚 𝑛 𝑜 𝑝 𝑞 𝑟 𝑠 𝑡 𝑢 𝑣 𝑤 𝑥 𝑦 𝑧

ino <C-U>iA 𝐴
ino <C-U>iB 𝐵
ino <C-U>iC 𝐶
ino <C-U>iD 𝐷
ino <C-U>iE 𝐸
ino <C-U>iF 𝐹
ino <C-U>iG 𝐺
ino <C-U>iH 𝐻
ino <C-U>iI 𝐼
ino <C-U>iJ 𝐽
ino <C-U>iK 𝐾
ino <C-U>iL 𝐿
ino <C-U>iM 𝑀
ino <C-U>iN 𝑁
ino <C-U>iO 𝑂
ino <C-U>iP 𝑃
ino <C-U>iQ 𝑄
ino <C-U>iR 𝑅
ino <C-U>iS 𝑆
ino <C-U>iT 𝑇
ino <C-U>iU 𝑈
ino <C-U>iV 𝑉
ino <C-U>iW 𝑊
ino <C-U>iX 𝑋
ino <C-U>iY 𝑌
ino <C-U>iZ 𝑍
ino <C-U>ia 𝑎
ino <C-U>ib 𝑏
ino <C-U>ic 𝑐
ino <C-U>id 𝑑
ino <C-U>ie 𝑒
ino <C-U>if 𝑓
ino <C-U>ig 𝑔
ino <C-U>ih ℎ
ino <C-U>ii 𝑖
ino <C-U>ij 𝑗
ino <C-U>ik 𝑘
ino <C-U>il 𝑙
ino <C-U>im 𝑚
ino <C-U>in 𝑛
ino <C-U>io 𝑜
ino <C-U>ip 𝑝
ino <C-U>iq 𝑞
ino <C-U>ir 𝑟
ino <C-U>is 𝑠
ino <C-U>it 𝑡
ino <C-U>iu 𝑢
ino <C-U>iv 𝑣
ino <C-U>iw 𝑤
ino <C-U>ix 𝑥
ino <C-U>iy 𝑦
ino <C-U>iz 𝑧

" 𝑨 𝑩 𝑪 𝑫 𝑬 𝑭 𝑮 𝑯 𝑰 𝑱 𝑲 𝑳 𝑴 𝑵 𝑶 𝑷 𝑸 𝑹 𝑺 𝑻 𝑼 𝑽 𝑾 𝑿 𝒀 𝒁
" 𝒂 𝒃 𝒄 𝒅 𝒆 𝒇 𝒈 𝒉 𝒊 𝒋 𝒌 𝒍 𝒎 𝒏 𝒐 𝒑 𝒒 𝒓 𝒔 𝒕 𝒖 𝒗 𝒘 𝒙 𝒚 𝒛

ino <C-U>biA 𝑨
ino <C-U>biB 𝑩
ino <C-U>biC 𝑪
ino <C-U>biD 𝑫
ino <C-U>biE 𝑬
ino <C-U>biF 𝑭
ino <C-U>biG 𝑮
ino <C-U>biH 𝑯
ino <C-U>biI 𝑰
ino <C-U>biJ 𝑱
ino <C-U>biK 𝑲
ino <C-U>biL 𝑳
ino <C-U>biM 𝑴
ino <C-U>biN 𝑵
ino <C-U>biO 𝑶
ino <C-U>biP 𝑷
ino <C-U>biQ 𝑸
ino <C-U>biR 𝑹
ino <C-U>biS 𝑺
ino <C-U>biT 𝑻
ino <C-U>biU 𝑼
ino <C-U>biV 𝑽
ino <C-U>biW 𝑾
ino <C-U>biX 𝑿
ino <C-U>biY 𝒀
ino <C-U>biZ 𝒁
ino <C-U>bia 𝒂
ino <C-U>bib 𝒃
ino <C-U>bic 𝒄
ino <C-U>bid 𝒅
ino <C-U>bie 𝒆
ino <C-U>bif 𝒇
ino <C-U>big 𝒈
ino <C-U>bih 𝒉
ino <C-U>bii 𝒊
ino <C-U>bij 𝒋
ino <C-U>bik 𝒌
ino <C-U>bil 𝒍
ino <C-U>bim 𝒎
ino <C-U>bin 𝒏
ino <C-U>bio 𝒐
ino <C-U>bip 𝒑
ino <C-U>biq 𝒒
ino <C-U>bir 𝒓
ino <C-U>bis 𝒔
ino <C-U>bit 𝒕
ino <C-U>biu 𝒖
ino <C-U>biv 𝒗
ino <C-U>biw 𝒘
ino <C-U>bix 𝒙
ino <C-U>biy 𝒚
ino <C-U>biz 𝒛

" 𝒜   𝒞 𝒟     𝒢     𝒥 𝒦     𝒩 𝒪 𝒫 𝒬   𝒮 𝒯 𝒰 𝒱 𝒲 𝒳 𝒴 𝒵
" 𝒶 𝒷 𝒸 𝒹   𝒻   𝒽 𝒾 𝒿 𝓀   𝓂 𝓃   𝓅 𝓆 𝓇 𝓈 𝓉 𝓊 𝓋 𝓌 𝓍 𝓎 𝓏

ino <C-U>sA 𝒜
ino <C-U>sB ℬ
ino <C-U>sC 𝒞
ino <C-U>sD 𝒟
ino <C-U>sE ℰ
ino <C-U>sF ℱ
ino <C-U>sG 𝒢
ino <C-U>sH ℋ
ino <C-U>sI ℐ
ino <C-U>sJ 𝒥
ino <C-U>sK 𝒦
ino <C-U>sL ℒ
ino <C-U>sM ℳ
ino <C-U>sN 𝒩
ino <C-U>sO 𝒪
ino <C-U>sP 𝒫
ino <C-U>sQ 𝒬
ino <C-U>sR ℛ
ino <C-U>sS 𝒮
ino <C-U>sT 𝒯
ino <C-U>sU 𝒰
ino <C-U>sV 𝒱
ino <C-U>sW 𝒲
ino <C-U>sX 𝒳
ino <C-U>sY 𝒴
ino <C-U>sZ 𝒵
ino <C-U>sa 𝒶
ino <C-U>sb 𝒷
ino <C-U>sc 𝒸
ino <C-U>sd 𝒹
ino <C-U>se ℯ
ino <C-U>sf 𝒻
ino <C-U>sg ℊ
ino <C-U>sh 𝒽
ino <C-U>si 𝒾
ino <C-U>sj 𝒿
ino <C-U>sk 𝓀
ino <C-U>sl ℓ
ino <C-U>sm 𝓂
ino <C-U>sn 𝓃
ino <C-U>so ℴ
ino <C-U>sp 𝓅
ino <C-U>sq 𝓆
ino <C-U>sr 𝓇
ino <C-U>ss 𝓈
ino <C-U>st 𝓉
ino <C-U>su 𝓊
ino <C-U>sv 𝓋
ino <C-U>sw 𝓌
ino <C-U>sx 𝓍
ino <C-U>sy 𝓎
ino <C-U>sz 𝓏

" 𝓐 𝓑 𝓒 𝓓 𝓔 𝓕 𝓖 𝓗 𝓘 𝓙 𝓚 𝓛 𝓜 𝓝 𝓞 𝓟 𝓠 𝓡 𝓢 𝓣 𝓤 𝓥 𝓦 𝓧 𝓨 𝓩
" 𝓪 𝓫 𝓬 𝓭 𝓮 𝓯 𝓰 𝓱 𝓲 𝓳 𝓴 𝓵 𝓶 𝓷 𝓸 𝓹 𝓺 𝓻 𝓼 𝓽 𝓾 𝓿 𝔀 𝔁 𝔂 𝔃

ino <C-U>sbA 𝓐
ino <C-U>sbB 𝓑
ino <C-U>sbC 𝓒
ino <C-U>sbD 𝓓
ino <C-U>sbE 𝓔
ino <C-U>sbF 𝓕
ino <C-U>sbG 𝓖
ino <C-U>sbH 𝓗
ino <C-U>sbI 𝓘
ino <C-U>sbJ 𝓙
ino <C-U>sbK 𝓚
ino <C-U>sbL 𝓛
ino <C-U>sbM 𝓜
ino <C-U>sbN 𝓝
ino <C-U>sbO 𝓞
ino <C-U>sbP 𝓟
ino <C-U>sbQ 𝓠
ino <C-U>sbR 𝓡
ino <C-U>sbS 𝓢
ino <C-U>sbT 𝓣
ino <C-U>sbU 𝓤
ino <C-U>sbV 𝓥
ino <C-U>sbW 𝓦
ino <C-U>sbX 𝓧
ino <C-U>sbY 𝓨
ino <C-U>sbZ 𝓩
ino <C-U>sba 𝓪
ino <C-U>sbb 𝓫
ino <C-U>sbc 𝓬
ino <C-U>sbd 𝓭
ino <C-U>sbe 𝓮
ino <C-U>sbf 𝓯
ino <C-U>sbg 𝓰
ino <C-U>sbh 𝓱
ino <C-U>sbi 𝓲
ino <C-U>sbj 𝓳
ino <C-U>sbk 𝓴
ino <C-U>sbl 𝓵
ino <C-U>sbm 𝓶
ino <C-U>sbn 𝓷
ino <C-U>sbo 𝓸
ino <C-U>sbp 𝓹
ino <C-U>sbq 𝓺
ino <C-U>sbr 𝓻
ino <C-U>sbs 𝓼
ino <C-U>sbt 𝓽
ino <C-U>sbu 𝓾
ino <C-U>sbv 𝓿
ino <C-U>sbw 𝔀
ino <C-U>sbx 𝔁
ino <C-U>sby 𝔂
ino <C-U>sbz 𝔃

" 𝔸 𝔹   𝔻 𝔼 𝔽 𝔾   𝕀 𝕁 𝕂 𝕃 𝕄   𝕆       𝕊 𝕋 𝕌 𝕍 𝕎 𝕏 𝕐
" 𝕒 𝕓 𝕔 𝕕 𝕖 𝕗 𝕘 𝕙 𝕚 𝕛 𝕜 𝕝 𝕞 𝕟 𝕠 𝕡 𝕢 𝕣 𝕤 𝕥 𝕦 𝕧 𝕨 𝕩 𝕪 𝕫

ino <C-U>[A 𝔸
ino <C-U>[B 𝔹
ino <C-U>[C ℂ
ino <C-U>[D 𝔻
ino <C-U>[E 𝔼
ino <C-U>[F 𝔽
ino <C-U>[G 𝔾
ino <C-U>[H ℍ
ino <C-U>[H ℍ
ino <C-U>[I 𝕀
ino <C-U>[J 𝕁
ino <C-U>[K 𝕂
ino <C-U>[L 𝕃
ino <C-U>[M 𝕄
ino <C-U>[N ℕ
ino <C-U>[O 𝕆
ino <C-U>[P ℙ
ino <C-U>[Q ℚ
ino <C-U>[R ℝ
ino <C-U>[S 𝕊
ino <C-U>[T 𝕋
ino <C-U>[U 𝕌
ino <C-U>[V 𝕍
ino <C-U>[W 𝕎
ino <C-U>[X 𝕏
ino <C-U>[Y 𝕐
ino <C-U>[Z ℤ
ino <C-U>[a 𝕒
ino <C-U>[b 𝕓
ino <C-U>[c 𝕔
ino <C-U>[d 𝕕
ino <C-U>[e 𝕖
ino <C-U>[f 𝕗
ino <C-U>[g 𝕘
ino <C-U>[h 𝕙
ino <C-U>[i 𝕚
ino <C-U>[j 𝕛
ino <C-U>[k 𝕜
ino <C-U>[l 𝕝
ino <C-U>[m 𝕞
ino <C-U>[n 𝕟
ino <C-U>[o 𝕠
ino <C-U>[p 𝕡
ino <C-U>[q 𝕢
ino <C-U>[r 𝕣
ino <C-U>[s 𝕤
ino <C-U>[t 𝕥
ino <C-U>[u 𝕦
ino <C-U>[v 𝕧
ino <C-U>[w 𝕨
ino <C-U>[x 𝕩
ino <C-U>[y 𝕪
ino <C-U>[z 𝕫

" 𝚨 𝚩 𝚪 𝚫 𝚬 𝚭 𝚮 𝚯 𝚰 𝚱 𝚲 𝚳 𝚴 𝚵 𝚶 𝚷 𝚸 𝚹 𝚺 𝚻 𝚼 𝚽 𝚾 𝚿 𝛀 𝛁
" 𝛂 𝛃 𝛄 𝛅 𝛆 𝛇 𝛈 𝛉 𝛊 𝛋 𝛌 𝛍 𝛎 𝛏 𝛐 𝛑 𝛒 𝛓 𝛔 𝛕 𝛖 𝛗 𝛘 𝛙 𝛚 𝛛 𝛜 𝛝 𝛞 𝛟 𝛠 𝛡

ino <C-U>bAlpha 𝚨
ino <C-U>bBeta 𝚩
ino <C-U>bGamma 𝚪
ino <C-U>bDelta 𝚫
ino <C-U>bEpsilon 𝚬
ino <C-U>bZeta 𝚭
ino <C-U>bEta 𝚮
ino <C-U>bTheta 𝚯
ino <C-U>bIota 𝚰
ino <C-U>bKappa 𝚱
ino <C-U>bLambda 𝚲
ino <C-U>bMu 𝚳
ino <C-U>bNu 𝚴
ino <C-U>bXi 𝚵
ino <C-U>bOmnicron 𝚶
ino <C-U>bPi 𝚷
ino <C-U>bRho 𝚸
ino <C-U>bSigma 𝚺
ino <C-U>bTau 𝚻
ino <C-U>bUpsilon 𝚼
ino <C-U>bPhi 𝚽
ino <C-U>bChi 𝚾
ino <C-U>bPsi 𝚿
ino <C-U>bOmega 𝛀
ino <C-U>bNabla 𝛁
ino <C-U>balpha 𝛂
ino <C-U>bbeta 𝛃
ino <C-U>bgamma 𝛄
ino <C-U>bdelta 𝛅
ino <C-U>bepsilon 𝛆
ino <C-U>bzeta 𝛇
" ino <C-U>beta 𝛈
ino <C-U>btheta 𝛉
ino <C-U>biota 𝛊
ino <C-U>bkappa 𝛋
ino <C-U>blambda 𝛌
ino <C-U>bmu 𝛍
ino <C-U>bnu 𝛎
ino <C-U>bxi 𝛏
ino <C-U>bomnicron 𝛐
ino <C-U>bpi 𝛑
ino <C-U>brho 𝛒
ino <C-U>bfsigma 𝛓
ino <C-U>bsigma 𝛔
ino <C-U>btau 𝛕
ino <C-U>bupsilon 𝛖
ino <C-U>bphi 𝛗
ino <C-U>bchi 𝛘
ino <C-U>bpsi 𝛙
ino <C-U>bomega 𝛚
ino <C-U>bpdiff 𝛛

" 𝛢 𝛣 𝛤 𝛥 𝛦 𝛧 𝛨 𝛩 𝛪 𝛫 𝛬 𝛭 𝛮 𝛯 𝛰 𝛱 𝛲 𝛳 𝛴 𝛵 𝛶 𝛷 𝛸 𝛹 𝛺 𝛻
" 𝛼 𝛽 𝛾 𝛿 𝜀 𝜁 𝜂 𝜃 𝜄 𝜅 𝜆 𝜇 𝜈 𝜉 𝜊 𝜋 𝜌 𝜍 𝜎 𝜏 𝜐 𝜑 𝜒 𝜓 𝜔 𝜕 𝜖 𝜗 𝜘 𝜙 𝜚 𝜛

ino <C-U>iAlpha 𝛢
ino <C-U>iBeta 𝛣
ino <C-U>iGamma 𝛤
ino <C-U>iDelta 𝛥
ino <C-U>iEpsilon 𝛦
ino <C-U>iZeta 𝛧
ino <C-U>iEta 𝛨
ino <C-U>iTheta 𝛩
ino <C-U>iIota 𝛪
ino <C-U>iKappa 𝛫
ino <C-U>iLambda 𝛬
ino <C-U>iMu 𝛭
ino <C-U>iNu 𝛮
ino <C-U>iXi 𝛯
ino <C-U>iOmnicron 𝛰
ino <C-U>iPi 𝛱
ino <C-U>iRho 𝛲
ino <C-U>iSigma 𝛴
ino <C-U>iTau 𝛵
ino <C-U>iUpsilon 𝛶
ino <C-U>iPhi 𝛷
ino <C-U>iChi 𝛸
ino <C-U>iPsi 𝛹
ino <C-U>iOmega 𝛺
ino <C-U>iNabla 𝛻
ino <C-U>ialpha 𝛼
ino <C-U>ibeta 𝛽
ino <C-U>igamma 𝛾
ino <C-U>idelta 𝛿
ino <C-U>iepsilon 𝜀
ino <C-U>izeta 𝜁
ino <C-U>ieta 𝜂
ino <C-U>itheta 𝜃
ino <C-U>iiota 𝜄
ino <C-U>ikappa 𝜅
ino <C-U>ilambda 𝜆
ino <C-U>imu 𝜇
ino <C-U>inu 𝜈
ino <C-U>ixi 𝜉
ino <C-U>iomnicron 𝜊
ino <C-U>ipi 𝜋
ino <C-U>irho 𝜌
ino <C-U>ifsigma 𝜍
ino <C-U>isigma 𝜎
ino <C-U>itau 𝜏
ino <C-U>iupsilon 𝜐
ino <C-U>iphi 𝜑
ino <C-U>ichi 𝜒
ino <C-U>ipsi 𝜓
ino <C-U>iomega 𝜔
ino <C-U>ipdiff 𝜕

" 𝜜 𝜝 𝜞 𝜟 𝜠 𝜡 𝜢 𝜣 𝜤 𝜥 𝜦 𝜧 𝜨 𝜩 𝜪 𝜫 𝜬 𝜭 𝜮 𝜯 𝜰 𝜱 𝜲 𝜳 𝜴 𝜵
" 𝜶 𝜷 𝜸 𝜹 𝜺 𝜻 𝜼 𝜽 𝜾 𝜿 𝝀 𝝁 𝝂 𝝃 𝝄 𝝅 𝝆 𝝇 𝝈 𝝉 𝝊 𝝋 𝝌 𝝍 𝝎 𝝏 𝝐 𝝑 𝝒 𝝓 𝝔 𝝕

ino <C-U>biAlpha 𝜜
ino <C-U>biBeta 𝜝
ino <C-U>biGamma 𝜞
ino <C-U>biDelta 𝜟
ino <C-U>biEpsilon 𝜠
ino <C-U>biZeta 𝜡
ino <C-U>biEta 𝜢
ino <C-U>biTheta 𝜣
ino <C-U>biIota 𝜤
ino <C-U>biKappa 𝜥
ino <C-U>biLambda 𝜦
ino <C-U>biMu 𝜧
ino <C-U>biNu 𝜨
ino <C-U>biXi 𝜩
ino <C-U>biOmnicron 𝜪
ino <C-U>biPi 𝜫
ino <C-U>biRho 𝜬
ino <C-U>biSigma 𝜮
ino <C-U>biTau 𝜯
ino <C-U>biUpsilon 𝜰
ino <C-U>biPhi 𝜱
ino <C-U>biChi 𝜲
ino <C-U>biPsi 𝜳
ino <C-U>biOmega 𝜴
ino <C-U>biNabla 𝜵
ino <C-U>bialpha 𝜶
ino <C-U>bibeta 𝜷
ino <C-U>bigamma 𝜸
ino <C-U>bidelta 𝜹
ino <C-U>biepsilon 𝜺
ino <C-U>bizeta 𝜻
ino <C-U>bieta 𝜼
ino <C-U>bitheta 𝜽
ino <C-U>biiota 𝜾
ino <C-U>bikappa 𝜿
ino <C-U>bilambda 𝝀
ino <C-U>bimu 𝝁
ino <C-U>binu 𝝂
ino <C-U>bixi 𝝃
ino <C-U>biomnicron 𝝄
ino <C-U>bipi 𝝅
ino <C-U>birho 𝝆
ino <C-U>bifsigma 𝝇
ino <C-U>bisigma 𝝈
ino <C-U>bitau 𝝉
ino <C-U>biupsilon 𝝊
ino <C-U>biphi 𝝋
ino <C-U>bichi 𝝌
ino <C-U>bipsi 𝝍
ino <C-U>biomega 𝝎
ino <C-U>bipdiff 𝝏

" 𝟎 𝟏 𝟐 𝟑 𝟒 𝟓 𝟔 𝟕 𝟖 𝟗
" 𝟘 𝟙 𝟚 𝟛 𝟜 𝟝 𝟞 𝟟 𝟠 𝟡

ino <C-U>b0 𝟎
ino <C-U>b1 𝟏
ino <C-U>b2 𝟐
ino <C-U>b3 𝟑
ino <C-U>b4 𝟒
ino <C-U>b5 𝟓
ino <C-U>b6 𝟔
ino <C-U>b7 𝟕
ino <C-U>b8 𝟖
ino <C-U>b9 𝟗
ino <C-U>[0 𝟘
ino <C-U>[1 𝟙
ino <C-U>[2 𝟚
ino <C-U>[3 𝟛
ino <C-U>[4 𝟜
ino <C-U>[5 𝟝
ino <C-U>[6 𝟞
ino <C-U>[7 𝟟
ino <C-U>[8 𝟠
ino <C-U>[9 𝟡

"       ▼  Suppl. Arrows-C
" U+1F800  🠀 🠁 🠂 🠃 🠄 🠅 🠆 🠇 🠈 🠉 🠊 🠋
" U+1F810  🠐 🠑 🠒 🠓 🠔 🠕 🠖 🠗 🠘 🠙 🠚 🠛 🠜 🠝 🠞 🠟
" U+1F820  🠠 🠡 🠢 🠣 🠤 🠥 🠦 🠧 🠨 🠩 🠪 🠫 🠬 🠭 🠮 🠯
" U+1F830  🠰 🠱 🠲 🠳 🠴 🠵 🠶 🠷 🠸 🠹 🠺 🠻 🠼 🠽 🠾 🠿
" U+1F840  🡀 🡁 🡂 🡃 🡄 🡅 🡆 🡇
" U+1F850  🡐 🡑 🡒 🡓 🡔 🡗 🡘
" U+1F860  🡠 🡡 🡢 🡣 🡤 🡧 🡨 🡩 🡪 🡫 🡬 🡯
" U+1F870  🡰 🡱 🡲 🡳 🡴 🡷 🡸 🡹 🡺 🡻 🡼 🡿
" U+1F880  🢀 🢁 🢂 🢃 🢄 🢇
" U+1F890  🢐 🢑 🢒 🢓 🢔 🢕 🢖 🢗 🢘 🢙 🢚 🢛
" U+1F8A0  🢠 🢡 🢢 🢣 🢤 🢥 🢦 🢧 🢨 🢩 🢪 🢫
