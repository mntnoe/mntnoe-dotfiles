" File:     text.vim
" Author:   Mads N Noe <mntnoe (@) gmail.com>
" Licence:  as-is
" Modified: 2010-11-22

if exists("b:current_syntax")
    finish
endif

syntax sync minlines=32

highlight Heading ctermfg=15 guifg=#000000 gui=bold
highlight List ctermfg=14 guifg=#009090

syntax match Heading /^.\+\n[=-]\{3,}$/
" Foo    Foo
" === or ---
syntax match Heading /^\( \+\).\{3,}\n\1[=-]\{3,}\n\%( \+.\+\n\)*$/
"           Foo
"           ===
"           Bar
"
syntax match Heading /^- .\+ -$/
" - Foo -

syntax match List /^\%(\s\+[\*-]\s\+.\+\|[\*-]\s\+.*[^-]\)$/
" * foo
"   - bar
"   - baz

let b:current_syntax="text"

" vim: set ft=vim:
