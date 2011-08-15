" File:     autoreload.vim
" Author:   Mads Navntoft Noe <mail@madsnoe.dk>
" Modified: 2010-04-10

if exists("loaded_autoreload")
    finish
endif
if (v:progname == "ex")
   finish
endif
let loaded_autoreload = 1

let g:AutoReload = 0
command! ToggleAutoReload let g:AutoReload=!g:AutoReload

autocmd BufWritePost *.htm call AutoReload()
autocmd BufWritePost *.html call AutoReload()
autocmd BufWritePost *.js call AutoReload()
autocmd BufWritePost *.php call AutoReload()
autocmd BufWritePost *.js call AutoReload()
autocmd BufWritePost *.css call AutoReloadCSS()

function! AutoReload()
    if !g:AutoReload | return
    endif
    
    silent !firefox -reload
endfunction

function! AutoReloadCSS()
    if !g:AutoReload | return
    endif
    
    silent !firefox -reload-css
endfunction

