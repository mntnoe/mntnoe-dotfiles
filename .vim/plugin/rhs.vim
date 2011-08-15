" File:     rhs.vim
" Author:   Mads N Noe <mntnoe (@) gmail.com>
" Licence:  as-is
" Modified: 2009-02-03

finish

" prelude {{{
if exists("loaded_rhs")
    finish
endif
if (v:progname == "ex")
   finish
endif
let loaded_rhs = 1
" }}}

" rhs_fixed {{{
if !exists("g:rhs_fixed")
    let g:rhs_fixed = 29
endif

function! s:RHSGetFixed()
    if exists("b:rhs_fixed")
        return b:rhs_fixed
    else
        return g:rhs_fixed
    endif
endfunction
" }}}

" rhs_patterns {{{
if !exists("g:rhs_patterns")
    let s:pt_spaces         = ['\%(^\|[^>=]\)\zs\s*::\ze\%($\|[^=]\)', '::']
    let s:pt_equal          = ['\%(^\|[^>=]\)\zs\s*=\ze\%($\|[^=]\)', '=']
    let s:pt_colon_colon    = ['\s*\%(\S\+\s\)\zs\s\+', '']
    let g:rhs_patterns      = [s:pt_spaces, s:pt_equal, s:pt_colon_colon]
endif

function! s:RHSGetPatterns()
    if exists("b:rhs_patterns")
        return b:rhs_patterns
    else
        return g:rhs_patterns
    endif
endfunction
" }}}

noremap <M-u> <Nop>
inoremap <M-u> <Nop>

" RHSUpdateFixed {{{
" Align the first occurrence of a pattern in 'pts' to column 'dest'
function! <SID>RHSUpdateFixed()
    let pattern = []
    let pos     = 9999
    let line    = getline(line("."))
    for try_pt in s:RHSGetPatterns()
        let try_pos = match(line, try_pt[0]) 
        if  try_pos != -1 && try_pos < pos
            " match() returns an index starting from 0
            let pos     = try_pos + 1
            let pattern = try_pt
        endif
    endfor

    if pos == 9999 | return | endif

    " keep at least one space before the pattern
    let spaces  = repeat(" ", s:RHSGetFixed() - pos - 1)
    let newline = substitute(line, pattern[0], " ".spaces.pattern[1], "")
    if  newline != line
        call setline(line("."), newline)
    endif
endfunction
" }}}
noremap  <silent> <M-u><M-u>     :call <SID>RHSUpdateFixed()<Cr>
inoremap <silent> <M-u><M-u>     <C-o>:call <SID>RHSUpdateFixed()<Cr>
ounmap   <M-u>


" RHSAlignFixed {{{
function! <SID>RHSAlignFixed(...)
    let pos = 0
    if a:0 == 1 && a:1 == "normal"
        let pos = match(getline(line("."))[col(".")-1:], '\S')
        if pos == -1 | return "" | endif
    endif
    let diff = s:RHSGetFixed() - (col(".") + pos)
    return repeat(" ", diff)
endfunction
" }}}
nnoremap <expr> <M-u><M-r>       "i".<SID>RHSAlignFixed("normal")."\<Esc>l"
inoremap <expr> <M-u><M-r>       <SID>RHSAlignFixed()

" RHSStoreFixed() {{{
function! <SID>RHSStoreFixed()
    if s:RHSGetFixed() != virtcol(".")
        let b:rhs_fixed = virtcol(".")
        echo "RHS: Fixed alignment: ".b:rhs_fixed." (buffer)"
    endif
endfunction
" }}}
nnoremap <M-u><M-l>              :call <SID>RHSStoreFixed()<Cr>

" RHSGlobalFixed() {{{
function! <SID>RHSGlobalFixed()
    if exists("b:rhs_fixed")
        unlet b:rhs_fixed
        echo "RHS: Fixed alignment: ".g:rhs_fixed." (global)"
    endif
endfunction
" }}}
nnoremap <M-u><M-g>              :call <SID>RHSGlobalFixed()<Cr>

" RHSAlignFind {{{
function! <SID>RHSAlignFind(line, ...)
    let normal = 0
    let any    = 0
    for str in a:000
        if str == "normal" | let normal = 1 | endif
        if str == "any"    | let any    = 1 | endif
    endfor

    if any == 1
        let char = '\s\+\zs\S'
    else
        echo 'Match (space for \S): '
        let input = getchar()
        let char = nr2char(input)
        if  char == " "
            " handle <Space> specially: matches non-whitespace
            let char = '\s\+\zs\S'
        endif
    endif

    let line = getline(a:line)
    let pos  = match(line[col(".")-1:], char)
    if pos == -1 | return "" | endif

    if normal == 1
        exec "normal i\<C-r>=repeat(' ', ".(pos).")\<Cr>\<Esc>l"
    else
        return repeat(" ", pos)
    endif
endfunction
" }}}
nnoremap <C-Space>               :call <SID>RHSAlignFind(line(".")-1, "normal", "any")<Cr>
nnoremap <C-@>                   :call <SID>RHSAlignFind(line(".")-1, "normal", "any")<Cr>
inoremap <C-Space>               <C-r>=<SID>RHSAlignFind(line(".")-1, "any")<Cr>
inoremap <C-@>                   <C-r>=<SID>RHSAlignFind(line(".")-1, "any")<Cr>

nnoremap <M-u><M-Space>          :call <SID>RHSAlignFind(line(".")+1, "normal", "any")<Cr>
inoremap <M-u><M-Space>          <C-r>=<SID>RHSAlignFind(line(".")+1, "any")<Cr>
nnoremap <M-u><Space>            :call <SID>RHSAlignFind(line(".")+1, "normal", "any")<Cr>
inoremap <M-u><Space>            <C-r>=<SID>RHSAlignFind(line(".")+1, "any")<Cr>

nnoremap <M-u><M-j>              :call <SID>RHSAlignFind(line(".")+1, "normal")<Cr>
inoremap <M-u><M-j>              <C-r>=<SID>RHSAlignFind(line(".")+1)<Cr>
nnoremap <M-u><M-k>              :call <SID>RHSAlignFind(line(".")-1, "normal")<Cr>
inoremap <M-u><M-k>              <C-r>=<SID>RHSAlignFind(line(".")-1)<Cr>
nnoremap <M-u><M-K>              :call <SID>RHSAlignFind(line(".")-2, "normal")<Cr>
inoremap <M-u><M-K>              <C-r>=<SID>RHSAlignFind(line(".")-2)<Cr>

" vim: set ft=vim fdm=marker
