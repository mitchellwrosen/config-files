" Big default verb/action changes:
"
" * J = page down
" * K = page up
" * s = surround word
" * S = surround WORD
" * SS = surround line
" * U = redo
" * X = exchange (x in visual mode)
" * XX = exchange line
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

cal plug#begin('~/.vim/plugged')

" Language Server Protocol implementation
" Plug 'autozimu/LanguageClient-neovim', { 'branch': 'next', 'do': 'bash install.sh', 'for': 'haskell' }

" Fuzzy search source code, files, etc
" :help fzf-vim
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

" Align on words
Plug 'godlygeek/tabular'

" Highlight yanks
Plug 'machakann/vim-highlightedyank'

" Sign column
Plug 'mhinz/vim-signify'

" Vim quickfix improvements
Plug 'romainl/vim-qf'

" File browser, only load when used
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }

" Multiple cursors for quick and dirty renaming. Very handy.
Plug 'terryma/vim-multiple-cursors'

" Swap the location of two selections. Occasionally useful.
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

" Language-specific syntax highlighting and such
Plug 'ElmCast/elm-vim', { 'for': 'elm' }
Plug 'LnL7/vim-nix', { 'for': 'nix' }
Plug 'neovimhaskell/haskell-vim', { 'for': 'haskell' }
Plug 'purescript-contrib/purescript-vim', { 'for': 'purescript' }

Plug 'chriskempson/base16-vim'

Plug 'SirVer/ultisnips'

Plug 'vmchale/dhall-vim', { 'for': 'dhall' }

cal plug#end()
" Automatically calls syntax on, filetype plugin indent on

" ==============================================================================
" Basic settings
" ==============================================================================

" Colorscheme requires base15-shell, which writes ~/.vimrc_background
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
set gp=rg\ --vimgrep       " use rg to grep
set hid                    " don't abandon out-of-sight buffers
set ic                     " case-insensitive searching
set icm=split              " show live command substitutions
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

" Swap ; and ;
no ; :
no : ;

" Disable annoying command search 'q:' that I never use
map q: <Nop>

map Q @q

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

" Big JK to move around (very fast and common)
nn J <C-d>
nn K <C-u>
" Wean myself off <C-d>, <C-u> (though I still have to use these hotkeys in
" pagers and such...)
nn <C-d> <Nop>
nn <C-u> <Nop>

" Make Y yank to the end of line, similar to how C and D behave
nn Y y$

" U to redo, since U is not very useful by default. I like having undo and
" redo so similar.
nn U <C-r>
" Weaning myself of <C-R> to redo
nn <C-r> <Nop>

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

" [tabular]
" Space-a to align on the word under the cursor
nn <silent> <Space>a m`:execute "Tabularize /" . expand("<cWORD>")<CR>``

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
nn <expr> <Space>o (len(system('git rev-parse')) ? ':Files' : ':GFiles')."\<CR>"
" Space-f ("find") the word under the cursor
nn <Space>f :Rg <C-r><C-w><CR>
" Space-k (because it's a home-row key) to fuzzy-search buffers
nn <Space>k :Buffers<CR>

" [NERDTree]
nn <silent> <Space>n :NERDTreeToggle<CR>

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
ino <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
ino <expr> <CR> pumvisible() ? "\<C-y>" : "\<CR>"

ino <C-u> <Nop>

"       ▼  Controls and Latin-1 Suppl.
"  U+0020    ! " # $ % & ' ( ) * + , - . /
"  U+0030  0 1 2 3 4 5 6 7 8 9 : ; < = > ?
"  U+0040  @ A B C D E F G H I J K L M N O
"  U+0050  P Q R S T U V W X Y Z [ \ ] ^ _
"  U+0060  ` a b c d e f g h i j k l m n o
"  U+0070  p q r s t u v w x y z { | } ~
"  U+00A0    ¡ ¢ £ ¤ ¥ ¦ § ¨ © ª « ¬ ­ ® ¯
"  U+00B0  ° ± ² ³ ´ µ ¶ · ¸ ¹ º » ¼ ½ ¾ ¿
"  U+00C0  À Á Â Ã Ä Å Æ Ç È É Ê Ë Ì Í Î Ï
"  U+00D0  Ð Ñ Ò Ó Ô Õ Ö × Ø Ù Ú Û Ü Ý Þ ß
"  U+00E0  à á â ã ä å æ ç è é ê ë ì í î ï
"  U+00F0  ð ñ ò ó ô õ ö ÷ ø ù ú û ü ý þ ÿ

ino <C-u>i! ¡
ino <C-u>cent ¢
ino <C-u>pound £
ino <C-u>currency ¤
ino <C-u>yen ¥
ino <C-u>brokenbar ¦
ino <C-u>section §
ino <C-u>diaeresis ¨
ino <C-u>copyright ©
ino <C-u>fordinal ª
ino <C-u>ldaquote «
ino <C-u>not ¬
ino <C-u>softhyphen ­
ino <C-u>registered ®
ino <C-u>macron ¯
ino <C-u>degree °
ino <C-u>plusminus ±
ino <C-u>^2 ²
ino <C-u>^3 ³
ino <C-u>acute ´
ino <C-u>micro µ
ino <C-u>pilcrow ¶
ino <C-u>middledot ·
ino <C-u>cedilla ¸
ino <C-u>^1 ¹
ino <C-u>mordinal º
ino <C-u>rdaquote »
ino <C-u>1/4 ¼
ino <C-u>1/2 ½
ino <C-u>3/4 ¾
ino <C-u>i? ¿
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
" ino <C-u>gravea à
" ino <C-u>acutea á
" ino <C-u>circumflexa â
" ino <C-u>tildea ã
" ino <C-u>diaeresisa ä
" ino <C-u>ringa å
" ino <C-u>ae æ
" ino <C-u>cedillac ç
" ino <C-u>gravee è
" ino <C-u>acutee é
" ino <C-u>circumflexe ê
" ino <C-u>diaeresise ë
" ino <C-u>gravei ì
" ino <C-u>acutei í
" ino <C-u>circumflexi î
" ino <C-u>diaeresisi ï
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
"  U+0110  Đ đ Ē ē Ĕ ĕ Ė ė Ę ę Ě ě Ĝ ĝ Ğ ğ
"  U+0120  Ġ ġ Ģ ģ Ĥ ĥ Ħ ħ Ĩ ĩ Ī ī Ĭ ĭ Į į
"  U+0130  İ ı Ĳ ĳ Ĵ ĵ Ķ ķ ĸ Ĺ ĺ Ļ ļ Ľ ľ Ŀ
"  U+0140  ŀ Ł ł Ń ń Ņ ņ Ň ň ŉ Ŋ ŋ Ō ō Ŏ ŏ
"  U+0150  Ő ő Œ œ Ŕ ŕ Ŗ ŗ Ř ř Ś ś Ŝ ŝ Ş ş
"  U+0160  Š š Ţ ţ Ť ť Ŧ ŧ Ũ ũ Ū ū Ŭ ŭ Ů ů
"  U+0170  Ű ű Ų ų Ŵ ŵ Ŷ ŷ Ÿ Ź ź Ż ż Ž ž ſ

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
"  U+0370                ;
"  U+0380      ΄ ΅ Ά · Έ Ή Ί  Ό  Ύ Ώ
"  U+0390  ΐ Α Β Γ Δ Ε Ζ Η Θ Ι Κ Λ Μ Ν Ξ Ο
"  U+03A0  Π Ρ   Σ Τ Υ Φ Χ Ψ Ω Ϊ Ϋ ά έ ή ί
"  U+03B0  ΰ α β γ δ ε ζ η θ ι κ λ μ ν ξ ο
"  U+03C0  π ρ ς σ τ υ φ χ ψ ω ϊ ϋ ό ύ ώ
"  U+03D0  ϐ ϑ   ϕ ϖ               Ϝ ϝ
"  U+03F0  ϰ ϱ   ϵ

ino <C-u>Gamma Γ
ino <C-u>Delta Δ
ino <C-u>Theta Θ
ino <C-u>Lambda Λ
ino <C-u>Xi Ξ
ino <C-u>Pi Π
ino <C-u>Sigma Σ
ino <C-u>Upsilon Υ
ino <C-u>Phi Φ
ino <C-u>Psi Ψ
ino <C-u>Omega Ω
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
ino <C-u>pi π
ino <C-u>rho ρ
ino <C-u>sigma σ
ino <C-u>tau τ
ino <C-u>upsilon υ
ino <C-u>phi φ
ino <C-u>chi χ
ino <C-u>psi ψ
ino <C-u>omega ω

"       ▼  Phonetic extensions
"  U+1D00  ᴀ ᴁ ᴂ ᴃ ᴄ ᴅ ᴆ ᴇ ᴈ ᴉ ᴊ ᴋ ᴌ ᴍ ᴎ ᴏ
"  U+1D10  ᴐ ᴑ ᴒ ᴓ ᴔ ᴕ ᴖ ᴗ ᴘ ᴙ ᴚ ᴛ ᴜ ᴝ ᴞ ᴟ
"  U+1D20  ᴠ ᴡ ᴢ ᴣ ᴤ ᴥ ᴦ ᴧ ᴨ ᴩ ᴪ ᴫ ᴬ ᴭ ᴮ ᴯ
"  U+1D30  ᴰ ᴱ ᴲ ᴳ ᴴ ᴵ ᴶ ᴷ ᴸ ᴹ ᴺ ᴻ ᴼ ᴽ ᴾ ᴿ
"  U+1D40  ᵀ ᵁ ᵂ ᵃ ᵄ ᵅ ᵆ ᵇ ᵈ ᵉ ᵊ ᵋ ᵌ ᵍ ᵎ ᵏ
"  U+1D50  ᵐ ᵑ ᵒ ᵓ ᵔ ᵕ ᵖ ᵗ ᵘ ᵙ ᵚ ᵛ ᵜ ᵝ ᵞ ᵟ
"  U+1D60  ᵠ ᵡ ᵢ ᵣ ᵤ ᵥ ᵦ ᵧ ᵨ ᵩ ᵪ ᵫ ᵬ ᵭ ᵮ ᵯ
"  U+1D70  ᵰ ᵱ ᵲ ᵳ ᵴ ᵵ ᵶ ᵷ ᵸ ᵹ ᵺ ᵻ ᵼ ᵽ ᵾ ᵿ

ino <C-u>^A ᴬ
ino <C-u>^B ᴮ
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
ino <C-u>^m ᵐ
ino <C-u>^o ᵒ
ino <C-u>^p ᵖ
ino <C-u>^t ᵗ
ino <C-u>^u ᵘ
ino <C-u>^v ᵛ
ino <C-u>^beta ᵝ
ino <C-u>^gamma ᵞ
ino <C-u>^delta ᵟ
ino <C-u>^phi ᵠ
ino <C-u>^chi ᵡ
ino <C-u>subi ᵢ
ino <C-u>subr ᵣ
ino <C-u>subu ᵤ
ino <C-u>subv ᵥ
ino <C-u>subbeta ᵦ
ino <C-u>subgamma ᵧ
ino <C-u>subrho ᵨ
ino <C-u>subphi ᵩ
ino <C-u>subchi ᵪ

"       ▼  General Punctuation
"  U+2000
"  U+2010  ‐ ‑ ‒ – — ― ‖ ‗ ‘ ’ ‚ ‛ “ ” „ ‟
"  U+2020  † ‡ • ‣ ․ ‥ … ‧
"  U+2030  ‰ ‱ ′ ″ ‴ ‵ ‶ ‷ ‸ ‹ › ※ ‼ ‽ ‾ ‿
"  U+2040  ⁀ ⁁ ⁂ ⁃ ⁄ ⁅ ⁆ ⁇ ⁈ ⁉ ⁊ ⁋ ⁌ ⁍ ⁎ ⁏
"  U+2050  ⁐ ⁑ ⁒ ⁓ ⁔ ⁕ ⁖ ⁗ ⁘ ⁙ ⁚ ⁛ ⁜ ⁝ ⁞
"  U+2060

"       ▼  Superscripts and Subscripts
"  U+2070  ⁰ ⁱ   ⁴ ⁵ ⁶ ⁷ ⁸ ⁹ ⁺ ⁻ ⁼ ⁽ ⁾ ⁿ
"  U+2080  ₀ ₁ ₂ ₃ ₄ ₅ ₆ ₇ ₈ ₉ ₊ ₋ ₌ ₍ ₎
"  U+2090  ₐ ₑ ₒ ₓ ₔ ₕ ₖ ₗ ₘ ₙ ₚ ₛ ₜ

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
"  U+2110  ℐ ℑ ℒ ℓ ℔ ℕ № ℗ ℘ ℙ ℚ ℛ ℜ ℝ ℞ ℟
"  U+2120  ℠ ℡ ™ ℣ ℤ ℥ Ω ℧ ℨ ℩ K Å ℬ ℭ ℮ ℯ
"  U+2130  ℰ ℱ Ⅎ ℳ ℴ ℵ ℶ ℷ ℸ ℹ ℺ ℻ ℼ ℽ ℾ ℿ
"  U+2140  ⅀ ⅁ ⅂ ⅃ ⅄ ⅅ ⅆ ⅇ ⅈ ⅉ ⅊ ⅋ ⅌ ⅍ ⅎ ⅏

ino <C-U>[C ℂ
ino <C-U>euler ℇ
ino <C-U>scriptg ℊ
ino <C-U>scriptH ℋ
ino <C-U>[H ℍ
ino <C-U>scriptI ℐ
ino <C-U>scriptL ℒ
ino <C-U>scriptl ℓ
ino <C-U>[N ℕ
ino <C-U>scriptP ℘
ino <C-U>[P ℙ
ino <C-U>[Q ℚ
ino <C-U>scriptR ℛ
ino <C-U>[R ℝ
ino <C-U>tm ™
ino <C-U>[Z ℤ
ino <C-U>scriptB ℬ
ino <C-U>scripte ℯ
ino <C-U>scriptE ℰ
ino <C-U>scriptF ℱ
ino <C-U>scriptM ℳ
ino <C-U>scripto ℴ
ino <C-U>info ℹ
ino <C-U>[pi ℼ
ino <C-U>[gamma ℽ
ino <C-U>[Gamma ℾ
ino <C-U>[Pi ℿ
ino <C-U>[nsumm ⅀
ino <C-U>[/D ⅅ
ino <C-U>[/d ⅆ
ino <C-U>[/e ⅇ
ino <C-U>[/i ⅈ
ino <C-U>[/j ⅉ

"       ▼  Number Forms
"  U+2150  ⅐ ⅑ ⅒ ⅓ ⅔ ⅕ ⅖ ⅗ ⅘ ⅙ ⅚ ⅛ ⅜ ⅝ ⅞ ⅟
"  U+2160  Ⅰ Ⅱ Ⅲ Ⅳ Ⅴ Ⅵ Ⅶ Ⅷ Ⅸ Ⅹ Ⅺ Ⅻ Ⅼ Ⅽ Ⅾ Ⅿ
"  U+2170  ⅰ ⅱ ⅲ ⅳ ⅴ ⅵ ⅶ ⅷ ⅸ ⅹ ⅺ ⅻ ⅼ ⅽ ⅾ ⅿ
"  U+2180  ↀ ↁ ↂ Ↄ ↄ ↅ ↆ ↇ ↈ ↉ ↊ ↋

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

"       ▼  Arrows
"  U+2190  ← ↑ → ↓ ↔ ↕ ↖ ↗ ↘ ↙ ↚ ↛ ↜ ↝ ↞ ↟
"  U+21A0  ↠ ↡ ↢ ↣ ↤ ↥ ↦ ↧ ↨ ↩ ↪ ↫ ↬ ↭ ↮ ↯
"  U+21B0  ↰ ↱ ↲ ↳ ↴ ↵ ↶ ↷ ↸ ↹ ↺ ↻ ↼ ↽ ↾ ↿
"  U+21C0  ⇀ ⇁ ⇂ ⇃ ⇄ ⇅ ⇆ ⇇ ⇈ ⇉ ⇊ ⇋ ⇌ ⇍ ⇎ ⇏
"  U+21D0  ⇐ ⇑ ⇒ ⇓ ⇔ ⇕ ⇖ ⇗ ⇘ ⇙ ⇚ ⇛ ⇜ ⇝ ⇞ ⇟
"  U+21E0  ⇠ ⇡ ⇢ ⇣ ⇤ ⇥ ⇦ ⇧ ⇨ ⇩ ⇪ ⇫ ⇬ ⇭ ⇮ ⇯
"  U+21F0  ⇰ ⇱ ⇲ ⇳ ⇴ ⇵ ⇶ ⇷ ⇸ ⇹ ⇺ ⇻ ⇼ ⇽ ⇾ ⇿

ino <C-U><- ←
ino <C-U>-^ ↑
ino <C-U>-> →
ino <C-U>-v ↓
ino <C-U><~ ↜
ino <C-U>~> ↝
ino <C-U><<- ↞
ino <C-U>-^^ ↟
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
ino <C-U><= ⇐
ino <C-U>=^ ⇑
ino <C-U>=> ⇒
ino <C-U>=v ⇓
ino <C-U><=> ⇔
ino <C-U>^=v ⇕

"       ▼  Mathematical Operators
"  U+2200  ∀ ∁ ∂ ∃ ∄ ∅ ∆ ∇ ∈ ∉ ∊ ∋ ∌ ∍ ∎ ∏
"  U+2210  ∐ ∑ − ∓ ∔ ∕ ∖ ∗ ∘ ∙ √ ∛ ∜ ∝ ∞ ∟
"  U+2220  ∠ ∡ ∢ ∣ ∤ ∥ ∦ ∧ ∨ ∩ ∪ ∫ ∬ ∭ ∮ ∯
"  U+2230  ∰ ∱ ∲ ∳ ∴ ∵ ∶ ∷ ∸ ∹ ∺ ∻ ∼ ∽ ∾ ∿
"  U+2240  ≀ ≁ ≂ ≃ ≄ ≅ ≆ ≇ ≈ ≉ ≊ ≋ ≌ ≍ ≎ ≏
"  U+2250  ≐ ≑ ≒ ≓ ≔ ≕ ≖ ≗ ≘ ≙ ≚ ≛ ≜ ≝ ≞ ≟
"  U+2260  ≠ ≡ ≢ ≣ ≤ ≥ ≦ ≧ ≨ ≩ ≪ ≫ ≬ ≭ ≮ ≯
"  U+2270  ≰ ≱ ≲ ≳ ≴ ≵ ≶ ≷ ≸ ≹ ≺ ≻ ≼ ≽ ≾ ≿
"  U+2280  ⊀ ⊁ ⊂ ⊃ ⊄ ⊅ ⊆ ⊇ ⊈ ⊉ ⊊ ⊋ ⊌ ⊍ ⊎ ⊏
"  U+2290  ⊐ ⊑ ⊒ ⊓ ⊔ ⊕ ⊖ ⊗ ⊘ ⊙ ⊚ ⊛ ⊜ ⊝ ⊞ ⊟
"  U+22A0  ⊠ ⊡ ⊢ ⊣ ⊤ ⊥ ⊦ ⊧ ⊨ ⊩ ⊪ ⊫ ⊬ ⊭ ⊮ ⊯
"  U+22B0  ⊰ ⊱ ⊲ ⊳ ⊴ ⊵ ⊶ ⊷ ⊸ ⊹ ⊺ ⊻ ⊼ ⊽ ⊾ ⊿
"  U+22C0  ⋀ ⋁ ⋂ ⋃ ⋄ ⋅ ⋆ ⋇ ⋈ ⋉ ⋊ ⋋ ⋌ ⋍ ⋎ ⋏
"  U+22D0  ⋐ ⋑ ⋒ ⋓ ⋔ ⋕ ⋖ ⋗ ⋘ ⋙ ⋚ ⋛ ⋜ ⋝ ⋞ ⋟
"  U+22E0  ⋠ ⋡ ⋢ ⋣ ⋤ ⋥ ⋦ ⋧ ⋨ ⋩ ⋪ ⋫ ⋬ ⋭ ⋮ ⋯
"  U+22F0  ⋰ ⋱ ⋲ ⋳ ⋴ ⋵ ⋶ ⋷ ⋸ ⋹ ⋺ ⋻ ⋼ ⋽ ⋾ ⋿

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
ino <C-u>ncoproduct ∐
ino <C-u>nsum ∑
ino <C-u>minus −
ino <C-u>minusplus ∓
ino <C-u>dotplus ∔
ino <C-u>divslash ∕
ino <C-u>setminus ∖
ino <C-u>asterisk ∗
ino <C-u>ring ∘
ino <C-u>. ∘
ino <C-u>bullet ∙
ino <C-u>root2 √
ino <C-u>root3 ∛
ino <C-u>root4 ∜
ino <C-u>proportional ∝
ino <C-u>infinity ∞
ino <C-u>rangle ∟
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
ino <C-U>:= ≔
ino <C-U>=: ≕
ino <C-U>def= ≝
ino <C-U>?= ≟
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
ino <C-U>/=< ≰
ino <C-U>/>= ≱
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
ino <C-U>sx ⊠
ino <C-U>s. ⊡
ino <C-U>top ⊤
ino <C-U>bottom ⊥
ino <C-U>-o ⊸
ino <C-U>xor ⊻
ino <C-U>nand ⊼
ino <C-U>nor ⊽
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
ino <C-U><<< ⋘
ino <C-U>>>> ⋙
ino <C-U>...<Bar> ⋮
ino <C-U>... ⋯
ino <C-U>.../ ⋰
ino <C-U>...\ ⋱

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

ino <C-U>^ ⌃
ino <C-U>v ⌄
ino <C-U>lceil ⌈
ino <C-U>rceil ⌉
ino <C-U>lfloor ⌊
ino <C-U>rfloor ⌋

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

"       ▼  Mathematical Alphan. Symbols
" U+1D400  𝐀 𝐁 𝐂 𝐃 𝐄 𝐅 𝐆 𝐇 𝐈 𝐉 𝐊 𝐋 𝐌 𝐍 𝐎 𝐏
" U+1D410  𝐐 𝐑 𝐒 𝐓 𝐔 𝐕 𝐖 𝐗 𝐘 𝐙 𝐚 𝐛 𝐜 𝐝 𝐞 𝐟
" U+1D420  𝐠 𝐡 𝐢 𝐣 𝐤 𝐥 𝐦 𝐧 𝐨 𝐩 𝐪 𝐫 𝐬 𝐭 𝐮 𝐯
" U+1D430  𝐰 𝐱 𝐲 𝐳 𝐴 𝐵 𝐶 𝐷 𝐸 𝐹 𝐺 𝐻 𝐼 𝐽 𝐾 𝐿
" U+1D440  𝑀 𝑁 𝑂 𝑃 𝑄 𝑅 𝑆 𝑇 𝑈 𝑉 𝑊 𝑋 𝑌 𝑍 𝑎 𝑏
" U+1D450  𝑐 𝑑 𝑒 𝑓 𝑔 𝑕 𝑖 𝑗 𝑘 𝑙 𝑚 𝑛 𝑜 𝑝 𝑞 𝑟
" U+1D460  𝑠 𝑡 𝑢 𝑣 𝑤 𝑥 𝑦 𝑧 𝑨 𝑩 𝑪 𝑫 𝑬 𝑭 𝑮 𝑯
" U+1D470  𝑰 𝑱 𝑲 𝑳 𝑴 𝑵 𝑶 𝑷 𝑸 𝑹 𝑺 𝑻 𝑼 𝑽 𝑾 𝑿
" U+1D480  𝒀 𝒁 𝒂 𝒃 𝒄 𝒅 𝒆 𝒇 𝒈 𝒉 𝒊 𝒋 𝒌 𝒍 𝒎 𝒏
" U+1D490  𝒐 𝒑 𝒒 𝒓 𝒔 𝒕 𝒖 𝒗 𝒘 𝒙 𝒚 𝒛 𝒜 𝒝 𝒞 𝒟
" U+1D4A0  𝒠 𝒡 𝒢 𝒣 𝒤 𝒥 𝒦 𝒧 𝒨 𝒩 𝒪 𝒫 𝒬 𝒭 𝒮 𝒯
" U+1D4B0  𝒰 𝒱 𝒲 𝒳 𝒴 𝒵 𝒶 𝒷 𝒸 𝒹 𝒺 𝒻   𝒽 𝒾 𝒿
" U+1D4C0  𝓀   𝓂 𝓃 𝓄 𝓅 𝓆 𝓇 𝓈 𝓉 𝓊 𝓋 𝓌 𝓍 𝓎 𝓏
" U+1D4D0  𝓐 𝓑 𝓒 𝓓 𝓔 𝓕 𝓖 𝓗 𝓘 𝓙 𝓚 𝓛 𝓜 𝓝 𝓞 𝓟
" U+1D4E0  𝓠 𝓡 𝓢 𝓣 𝓤 𝓥 𝓦 𝓧 𝓨 𝓩 𝓪 𝓫 𝓬 𝓭 𝓮 𝓯
" U+1D4F0  𝓰 𝓱 𝓲 𝓳 𝓴 𝓵 𝓶 𝓷 𝓸 𝓹 𝓺 𝓻 𝓼 𝓽 𝓾 𝓿
" U+1D500  𝔀 𝔁 𝔂 𝔃 𝔄 𝔅 𝔆 𝔇 𝔈 𝔉 𝔊 𝔋 𝔌 𝔍 𝔎 𝔏
" U+1D510  𝔐 𝔑 𝔒 𝔓 𝔔 𝔕 𝔖 𝔗 𝔘 𝔙 𝔚 𝔛 𝔜 𝔝 𝔞 𝔟
" U+1D520  𝔠 𝔡 𝔢 𝔣 𝔤 𝔥 𝔦 𝔧 𝔨 𝔩 𝔪 𝔫 𝔬 𝔭 𝔮 𝔯
" U+1D530  𝔰 𝔱 𝔲 𝔳 𝔴 𝔵 𝔶 𝔷 𝔸 𝔹   𝔻 𝔼 𝔽 𝔾
" U+1D540  𝕀 𝕁 𝕂 𝕃 𝕄   𝕆       𝕊 𝕋 𝕌 𝕍 𝕎 𝕏
" U+1D550  𝕐   𝕒 𝕓 𝕔 𝕕 𝕖 𝕗 𝕘 𝕙 𝕚 𝕛 𝕜 𝕝 𝕞 𝕟
" U+1D560  𝕠 𝕡 𝕢 𝕣 𝕤 𝕥 𝕦 𝕧 𝕨 𝕩 𝕪 𝕫 𝕬 𝕭 𝕮 𝕯
" U+1D570  𝕰 𝕱 𝕲 𝕳 𝕴 𝕵 𝕶 𝕷 𝕸 𝕹 𝕺 𝕻 𝕼 𝕽 𝕾 𝕿
" U+1D580  𝖀 𝖁 𝖂 𝖃 𝖄 𝖅 𝖆 𝖇 𝖈 𝖉 𝖊 𝖋 𝖌 𝖍 𝖎 𝖏
" U+1D590  𝖐 𝖑 𝖒 𝖓 𝖔 𝖕 𝖖 𝖗 𝖘 𝖙 𝖚 𝖛 𝖜 𝖝 𝖞 𝖟
" U+1D5A0  𝖠 𝖡 𝖢 𝖣 𝖤 𝖥 𝖦 𝖧 𝖨 𝖩 𝖪 𝖫 𝖬 𝖭 𝖮 𝖯
" U+1D5B0  𝖰 𝖱 𝖲 𝖳 𝖴 𝖵 𝖶 𝖷 𝖸 𝖹 𝖺 𝖻 𝖼 𝖽 𝖾 𝖿
" U+1D5C0  𝗀 𝗁 𝗂 𝗃 𝗄 𝗅 𝗆 𝗇 𝗈 𝗉 𝗊 𝗋 𝗌 𝗍 𝗎 𝗏
" U+1D5D0  𝗐 𝗑 𝗒 𝗓 𝗔 𝗕 𝗖 𝗗 𝗘 𝗙 𝗚 𝗛 𝗜 𝗝 𝗞 𝗟
" U+1D5E0  𝗠 𝗡 𝗢 𝗣 𝗤 𝗥 𝗦 𝗧 𝗨 𝗩 𝗪 𝗫 𝗬 𝗭 𝗮 𝗯
" U+1D5F0  𝗰 𝗱 𝗲 𝗳 𝗴 𝗵 𝗶 𝗷 𝗸 𝗹 𝗺 𝗻 𝗼 𝗽 𝗾 𝗿
" U+1D600  𝘀 𝘁 𝘂 𝘃 𝘄 𝘅 𝘆 𝘇 𝘈 𝘉 𝘊 𝘋 𝘌 𝘍 𝘎 𝘏
" U+1D610  𝘐 𝘑 𝘒 𝘓 𝘔 𝘕 𝘖 𝘗 𝘘 𝘙 𝘚 𝘛 𝘜 𝘝 𝘞 𝘟
" U+1D620  𝘠 𝘡 𝘢 𝘣 𝘤 𝘥 𝘦 𝘧 𝘨 𝘩 𝘪 𝘫 𝘬 𝘭 𝘮 𝘯
" U+1D630  𝘰 𝘱 𝘲 𝘳 𝘴 𝘵 𝘶 𝘷 𝘸 𝘹 𝘺 𝘻
" U+1D670  𝙰 𝙱 𝙲 𝙳 𝙴 𝙵 𝙶 𝙷 𝙸 𝙹 𝙺 𝙻 𝙼 𝙽 𝙾 𝙿
" U+1D680  𝚀 𝚁 𝚂 𝚃 𝚄 𝚅 𝚆 𝚇 𝚈 𝚉 𝚊 𝚋 𝚌 𝚍 𝚎 𝚏
" U+1D690  𝚐 𝚑 𝚒 𝚓 𝚔 𝚕 𝚖 𝚗 𝚘 𝚙 𝚚 𝚛 𝚜 𝚝 𝚞 𝚟
" U+1D6A0  𝚠 𝚡 𝚢 𝚣 𝚤 𝚥     𝚨 𝚩 𝚪 𝚫 𝚬 𝚭 𝚮 𝚯
" U+1D6B0  𝚰 𝚱 𝚲 𝚳 𝚴 𝚵 𝚶 𝚷 𝚸 𝚹 𝚺 𝚻 𝚼 𝚽 𝚾 𝚿
" U+1D6C0  𝛀 𝛁 𝛂 𝛃 𝛄 𝛅 𝛆 𝛇 𝛈 𝛉 𝛊 𝛋 𝛌 𝛍 𝛎 𝛏
" U+1D6D0  𝛐 𝛑 𝛒 𝛓 𝛔 𝛕 𝛖 𝛗 𝛘 𝛙 𝛚 𝛛 𝛜 𝛝 𝛞 𝛟
" U+1D6E0  𝛠 𝛡 𝛢 𝛣 𝛤 𝛥 𝛦 𝛧 𝛨 𝛩 𝛪 𝛫 𝛬 𝛭 𝛮 𝛯
" U+1D6F0  𝛰 𝛱 𝛲 𝛳 𝛴 𝛵 𝛶 𝛷 𝛸 𝛹 𝛺 𝛻 𝛼 𝛽 𝛾 𝛿
" U+1D700  𝜀 𝜁 𝜂 𝜃 𝜄 𝜅 𝜆 𝜇 𝜈 𝜉 𝜊 𝜋 𝜌 𝜍 𝜎 𝜏
" U+1D710  𝜐 𝜑 𝜒 𝜓 𝜔 𝜕 𝜖 𝜗 𝜘 𝜙 𝜚 𝜛 𝜜 𝜝 𝜞 𝜟
" U+1D720  𝜠 𝜡 𝜢 𝜣 𝜤 𝜥 𝜦 𝜧 𝜨 𝜩 𝜪 𝜫 𝜬 𝜭 𝜮 𝜯
" U+1D730  𝜰 𝜱 𝜲 𝜳 𝜴 𝜵 𝜶 𝜷 𝜸 𝜹 𝜺 𝜻 𝜼 𝜽 𝜾 𝜿
" U+1D740  𝝀 𝝁 𝝂 𝝃 𝝄 𝝅 𝝆 𝝇 𝝈 𝝉 𝝊 𝝋 𝝌 𝝍 𝝎 𝝏
" U+1D750  𝝐 𝝑 𝝒 𝝓 𝝔 𝝕 𝝖 𝝗 𝝘 𝝙 𝝚 𝝛 𝝜 𝝝 𝝞 𝝟
" U+1D760  𝝠 𝝡 𝝢 𝝣 𝝤 𝝥 𝝦 𝝧 𝝨 𝝩 𝝪 𝝫 𝝬 𝝭 𝝮 𝝯
" U+1D770  𝝰 𝝱 𝝲 𝝳 𝝴 𝝵 𝝶 𝝷 𝝸 𝝹 𝝺 𝝻 𝝼 𝝽 𝝾 𝝿
" U+1D780  𝞀 𝞁 𝞂 𝞃 𝞄 𝞅 𝞆 𝞇 𝞈 𝞉 𝞊 𝞋 𝞌 𝞍 𝞎 𝞏
" U+1D790  𝞐 𝞑 𝞒 𝞓 𝞔 𝞕 𝞖 𝞗 𝞘 𝞙 𝞚 𝞛 𝞜 𝞝 𝞞 𝞟
" U+1D7A0  𝞠 𝞡 𝞢 𝞣 𝞤 𝞥 𝞦 𝞧 𝞨 𝞩 𝞪 𝞫 𝞬 𝞭 𝞮 𝞯
" U+1D7B0  𝞰 𝞱 𝞲 𝞳 𝞴 𝞵 𝞶 𝞷 𝞸 𝞹 𝞺 𝞻 𝞼 𝞽 𝞾 𝞿
" U+1D7C0  𝟀 𝟁 𝟂 𝟃 𝟄 𝟅 𝟆 𝟇 𝟈 𝟉 𝟊 𝟋     𝟎 𝟏
" U+1D7D0  𝟐 𝟑 𝟒 𝟓 𝟔 𝟕 𝟖 𝟗 𝟘 𝟙 𝟚 𝟛 𝟜 𝟝 𝟞 𝟟
" U+1D7E0  𝟠 𝟡 𝟢 𝟣 𝟤 𝟥 𝟦 𝟧 𝟨 𝟩 𝟪 𝟫 𝟬 𝟭 𝟮 𝟯
" U+1D7F0  𝟰 𝟱 𝟲 𝟳 𝟴 𝟵 𝟶 𝟷 𝟸 𝟹 𝟺 𝟻 𝟼 𝟽 𝟾 𝟿

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

" ino <C-u>- ⁃
" ino <C-u>!! ‼
" ino <C-u>!? ⁉
" ino <C-u>?? ⁇
" ino <C-u>?! ⁈
" ino <C-u>* ⁎
" ino <C-u>** ⁑

" ------------------------------------------------------------------------------
" Visual mode
" ------------------------------------------------------------------------------

" Same as insert mode
xn J <C-d>
xn K <C-u>
xn <C-d> <Nop>
xn <C-u> <Nop>

" Make visual mode * work like normal mode *
vn * y/<C-r>"<CR>

" After yank, leave cursor at the end of the highlight
vn y y`]

" Ctrl+S to search-and-replace
xn <C-s> :s//g<Left><Left>

" [exchange]
" x to exchange. vd and vx have the same default behavior (delete), so we
" don't lose any functionality here.
xm x <Plug>(Exchange)

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

" [fzf.vim]
" Space-f ("find") the selected contents
vm <Space>f y:Ag <C-r>"<CR>

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

" Add :Rg command to call ripgrep
" (stolen from https://github.com/junegunn/fzf.vim#advanced-customization)
command! -bang -nargs=* Rg
  \ call fzf#vim#grep(
  \   'rg --column --line-number --no-heading --color=always --smart-case '.shellescape(<q-args>), 1,
  \   <bang>0 ? fzf#vim#with_preview('up:60%')
  \           : fzf#vim#with_preview('right:50%:hidden', '?'),
  \   <bang>0)

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

function! <SID>EchoQuickFixEntry()
  let entries = getqflist()
  let bufnr = bufnr('%')
  let lnum = line('.')
  for e in entries
    if e.bufnr == bufnr && e.lnum == lnum
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
" au CursorMoved * cal <SID>EchoQuickFixEntry()

" ------------------------------------------------------------------------------
" Command mode
" ------------------------------------------------------------------------------

" [UltiSnips]
" Edit snippets of current file type
ca snipedit UltiSnipsEdit

" ==============================================================================
" Plugin settings
" ==============================================================================

" [EasyMotion]
let g:EasyMotion_do_mapping = 0 " Don't make any key mappings
let g:EasyMotion_smartcase = 1
let g:EasyMotion_use_upper = 1
let g:EasyMotion_keys = 'ASDGHKLQWERTYUIOPZXCVBNMFJ;'

" [elm]
let g:elm_setup_keybindings = 0 " Don't make any key mappings
let g:elm_format_autosave = 1 " Run elm-format on save

" [exchange]
" Don't make any key mappings
let g:exchange_no_mappings = 1

" [fzf]
" If the buffer is already open in another tab or window, jump to it rather
" than replace the current buffer (which would open 2 copies)
let g:fzf_buffers_jump = 1

" [haskell]
let g:haskell_indent_disable = 1
let g:haskell_enable_backpack = 1
let g:haskell_enable_pattern_synonyms = 1
let g:haskell_enable_quantification = 1
let g:haskell_enable_recursivedo = 1
let g:haskell_enable_typeroles = 1

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

" [signify]
let g:signify_sign_change = 'Δ'
let g:signify_sign_delete = '-'
" I only use git, so only bother integrating with it (performance win!)
let g:signify_vcs_list = [ 'git' ]

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

au FileType fzf setl laststatus=0
  \| au BufLeave <buffer> setl laststatus=2
" Escape to quit little annoying temporary buffers
au FileType fzf nn <silent> <buffer> <Esc> :q<CR>

" Space-p to format Haskell code with stylish-haskell
au FileType haskell nn <buffer> <silent> <Space>p m`:%!stylish-haskell<CR>``
" Trying out ormolu as well
au FileType haskell nn <buffer> <silent> <Space>P m`:%!ormolu -c ormolu.yaml %<CR>``
" <Space>ff to find-function (ag can match over multiple lines)
" <Space>ft to find-type (ripgrep is faster)
au FileType haskell nn <buffer> <Space>ff :Ag (<Bslash>b)<C-r><C-w><Bslash>b[ <Bslash>t<Bslash>n]+::<CR>
au FileType haskell nn <buffer> <Space>ft :Rg (data<Bar>newtype<Bar>Type)( +)<Bslash>b<C-r><C-w><Bslash>b<CR>
" au FileType haskell nn <Space>p :cal LanguageClient_textDocument_formatting()<CR>
" Swap ; and : in Haskell, PureScript
au FileType elm,haskell,purescript ino ; :
au FileType elm,haskell,purescript ino : ;
au FileType elm,haskell,purescript nn r; r:
au FileType elm,haskell,purescript nn r: r;
au FileType haskell nn <Space>it m`"ayiwI<C-r>=system('cabal new-repl -v0 --repl-options=-fno-code --repl-options=-v0 2>/dev/null <<< ":t <C-r>a"')<CR><Esc>``

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
