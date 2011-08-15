" File:     dot.vim
" Author:   Mads N Noe <mntnoe (@) gmail.com>
" Licence:  as-is
" Modified: 2008-09-08

set smartindent

nnoremap <buffer> <Leader>m :up<Cr>:!dot -Tps:cairo % -o <C-r>=expand("%:r")<Cr>.ps<Cr>
nnoremap <buffer> <Leader>M :up<Cr>:!dot -Tpdf % -o <C-r>=expand("%:r")<Cr>.pdf<Cr>
nnoremap <buffer> <LocalLeader>v :!gv <C-r>=expand("%:r")<Cr>.ps &<Cr><Cr>

nnoremap <buffer> <LocalLeader>c :!for I in <C-r>=expand("%:p:h")<Cr>/*.dot; do dot -Tps:cairo $I -o ${I\%.dot}.ps && echo $I; done<Cr>
nnoremap <buffer> <LocalLeader>C :!for I in <C-r>=expand("%:p:h")<Cr>/*.dot; do dot -Tpdf $I -o ${I\%.dot}.pdf && echo $I; done<Cr>
" vim: set ft=vim:
