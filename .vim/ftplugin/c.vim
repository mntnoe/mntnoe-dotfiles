" File:     c.vim
" Author:   Mads N Noe <mntnoe (@) gmail.com>
" Licence:  as-is
" Modified: 2011-02-08

" don't indent public etc, and use brackets in switch/case
setlocal cinoptions=g0l1

setlocal commentstring=//\ %s

" do not run this for .cpp files:
if &filetype == "c" | setlocal tags+=~/.vim/help/c_std_tags
                      " we can't use setlocal
                      set runtimepath+=~/.vim/help/crefvim
endif

if &filetype == "c" | setlocal commentstring=/*\ \%s\ */ | endif

command! CTags !ctags --options=$HOME/.vim/extern/ctags-cpp -R .

" Generally, <M-m> compiles and <M-,> and <M-.> executes. 
nnoremap <buffer> <M-,> :!<C-r>=expand('%:p:r')<Cr>

nnoremap <buffer> <M-a> :A<Cr>

let g:OmniMake_cmds = ["make -j2 --no-print-directory", "cwindow"]

"set path+=/usr/lib/avr/include


function! ShowHelp_c(name)
    let is_gl  = strlen(a:name) >= 3 && a:name[0:1] ==# "gl" && a:name[2:2] <# "a" 
    let is_glu = strlen(a:name) >= 4 && a:name[0:2] ==# "glu" && a:name[3:3] <# "a"
    if ! (is_gl || is_glu)
        return 0
    end

    silent exec "!firefox 'http://www.opengl.org/sdk/docs/man/xhtml/".a:name.".xml' &"

    return 1
endfunction
let b:ShowHelp_func = "ShowHelp_c"


for i in ["c", "cpp"]
    call IMAP(":htpl", "#ifndef \<C-r>=substitute(toupper(expand('%:t')), '\\.', '_', 'g')\<Cr>\<Cr>#define \<C-r>=substitute(toupper(expand('%:t')), '\\.', '_', 'g')\<Cr>\<Cr>\<Cr><++>\<Cr>\<Cr>#endif // \<C-r>=substitute(toupper(expand('%:t')), '\\.', '_', 'g')\<Cr>", i)
    call IMAP(":ctpl", "#include \"\<C-r>=expand('%:t:r')\<Cr>.h\"", i)
endfor

nnoremap <buffer> <LocalLeader>i ?#include<Cr>o#include<Space>

" vim: set ft=vim fdm=marker fdl=0:
