" File:     cleanup.vim
" Author:   Mads N Noe <mntnoe (@) gmail.com>
" Licence:  as-is
" Modified: 2008-09-08
"
" Clean up mappings etc. created by other plugins

if maparg("<Leader>bug") != "" | unmap <Leader>bug
endif
if maparg("<Leader>be")  != "" | unmap <Leader>be
endif

" vim: set ft=vim:
