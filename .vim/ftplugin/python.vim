" File:     python.vim
" Author:   Mads N Noe <mntnoe (@) gmail.com>
" Licence:  as-is
" Modified: 2010-07-03

" make
setlocal errorformat=%C\ %.%#,%A\ \ File\ \"%f\"\\,\ line\ %l%.%#,%Z%[%^\ ]%\\@=%m 
setlocal makeprg=./%
" setlocal makeprg=./alltests.py\ -q

iabbr <buffer> sae self.assertEquals
iabbr <buffer> sar self.assertRaises

set keywordprg=pydoc
nnoremap <buffer> <F1> :!firefox http://localhost:7464/<C-R><C-W>.html &<Cr><Cr>


call IMAP(':sh', '#!/usr/bin/env python', 'python')
call IMAP(':i', 'from <++> import <++>', 'python')

" vim: set ft=vim:
