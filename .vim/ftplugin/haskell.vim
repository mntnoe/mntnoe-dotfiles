" File:     haskell.vim
" Author:   Mads N Noe <mntnoe (@) gmail.com>
" Licence:  as-is
" Modified: 2010-04-19

" mnemonic: execute
nnoremap <buffer> <M-,> :up<Cr>:GHCi main<Cr>

nnoremap <buffer> <M-J> {o-- \|<Space>

let s:header  = "--------------------------------------------------------------------\n"
let s:header .= "-- |\n"
let s:header .= "-- \n"
let s:header .= "-- Module      :  <++>\<C-r>=substitute(expand(\"%:r\"), \"/\", \".\", \"g\")\<Cr> \n"
let s:header .= "-- Copyright   :  (c) Mads N Noe 2009\n"
let s:header .= "-- Maintainer  :  mntnoe (@) gmail.com\n"
let s:header .= "-- License     :  as-is\n"
let s:header .= "-- \n"
let s:header .= "-- <+ description +>\n"
let s:header .= "-- \n"
let s:header .= "--------------------------------------------------------------------"
call IMAP(":hhdr", s:header, "haskell")

let s:app_templ  = "\n"
let s:app_templ .= "-- <+Title+>\n"
let s:app_templ .= ", nullApp\n"
let s:app_templ .= "  { cmd     = spawn \"<+app+>\"\n"
let s:app_templ .= ", appType = JumpTo\n"
let s:app_templ .= ", key     = (<+i, xK_+>)\n"
let s:app_templ .= ", query   = className =? \"<+Class+>\"\n"
let s:app_templ .= ", icon    = \"<+apps/app.xpm+>\"\n"
let s:app_templ .= "}\n"
call IMAP(":happ", s:app_templ, "haskell")

" vim: set ft=vim
