" File:     netrw.vim
" Author:   Mads N Noe <mntnoe (@) gmail.com>
" Modified: 2009-05-22

" Mark file and select next
nnoremap <buffer> f :<C-U>call <SNR>49_NetrwMarkFile(1,<SNR>49_NetrwGetWord())<CR>j

" Return to previous file.
nnoremap <buffer> <M-f> <C-^>

" vim: set ft=vim:
