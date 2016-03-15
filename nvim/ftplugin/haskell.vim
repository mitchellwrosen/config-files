setlocal ts=2
setlocal sts=2
setlocal sw=2

nnoremap <silent> <Enter> :nohlsearch<CR>:GhcModTypeClear<CR>

" ghc-mod
nmap <buffer> <silent> <leader>ht :GhcModType<CR>
nmap <buffer> <silent> <leader>hT :GhcModTypeInsert<CR>
nmap <buffer> <silent> <leader>hs :GhcModSplitFunCase<CR>

" neco-ghc
let g:haskellmode_completion_ghc = 0
let g:necoghc_enable_detailed_browse = 1
setlocal omnifunc=necoghc#omnifunc

" YouCompleteMe
let g:ycm_semantic_triggers = {'haskell': ['.']}

" neomake
" autocmd! BufWritePost * Neomake
