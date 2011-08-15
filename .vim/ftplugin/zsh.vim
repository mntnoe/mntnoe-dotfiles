" File:     zsh.vim
" Author:   Mads N Noe <mntnoe (@) gmail.com>
" Modified: 2010-03-23

call IMAP(":sh","#!/bin/zsh","zsh")
call IMAP(":err","error () {\necho $*\nexit 1\n}","zsh")
call IMAP(":noarg","[[ -z \"$*\" ]] && error \"Usage: $0:t <args>\"","zsh")

" vim: set ft=vim tw=78:
