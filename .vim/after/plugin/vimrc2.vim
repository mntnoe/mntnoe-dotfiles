" File:     vimrc2.vim
" Author:   Mads N Noe <mntnoe (@) gmail.com>
" Modified: 2011-03-25
"
" Configuration depending on loaded plugins

" InsertHeader() {{{
function! InsertHeader()
    let file     = "File:     ".expand("%:t")
    let author   = "Author:   Mads Navntoft Noe <mail@madsnoe.dk>"
    let modified = "Modified: __TIMESTAMP"
    let pad_cms  = g:pad_cms()
    return printf(pad_cms, file)."\n".printf(pad_cms, author)."\n".printf(pad_cms, modified)
endfunction
" }}}
call IMAP(":hdr", "\<C-r>=InsertHeader()\<Cr>", "")

" InsertFooter() {{{
function! InsertFooter()
    let ft  = ""
    let fdm = ""
    let fdl = ""
    let tw  = ""
    if  &ft != ""        | let ft = " ft=".&ft    | endif
    if  &fdm == "marker" | let fdm = " fdm=".&fdm | endif
    if  &fdl != 0        | let fdl = " fdl=".&fdl | endif
    if  &tw  != 0        | let tw  = " tw=".&tw   | endif
    let footer = "vim: set".ft.fdm.fdl.tw.":"
    return printf(g:pad_cms(), footer)
endfunction
" }}}
call IMAP(":ftr", "\<C-r>=InsertFooter()\<Cr>", "")

" Todos
call IMAP(":to", "\<C-r>=printf(g:pad_cms(), 'TODO: ')\<Cr>", "")
call IMAP(":fix", "\<C-r>=printf(g:pad_cms(), 'FIXME: ')\<Cr>", "")

call IMAP(":co", "# Copyright \<C-R>=strftime(\"%Y\")\<Cr> Adroit. All rights reserved.\<Cr>", "")

" Timestamp
call IMAP(":ts", "\<C-R>=strftime(\"%Y-%m-%d\")\<Cr>", "")
