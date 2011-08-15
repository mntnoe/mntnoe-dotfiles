" File:     todo.vim
" Author:   Mads N Noe <mntnoe (@) gmail.com>
" Modified: 2009-05-12


if exists("b:current_syntax")
    finish
endif

highlight link TodoDone Comment

syntax match TodoDone /^\*.*/
