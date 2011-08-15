" File:     cppindent.vim
" Author:   Mads N Noe <mntnoe (@) gmail.com>
" Modified: 2009-05-22
"
" Quick hack to cindent. Do not indent after namespace declarations.

if exists("b:did_cppindent")
"    finish
endif
let b:did_cppindent = 1

setlocal nolisp
setlocal noautoindent

setlocal indentexpr=GetCppIndent(v:lnum)

if exists("*GetCppIndent")
    finish
endif

function! GetCppIndent(lnum)
    let cindent = cindent(a:lnum)
    if a:lnum == 1 | return cindent | endif

    let pattern1 = 'namespace\s\+\S\+\s*{\s*\%$'
    " pattern2 is used to match this case:
    " class c : public b
    "     { <-- cursor
    let clspat = 'class\s\+\S\+\s*:\s*[^{]*'
    let pattern2 = 'namespace\s\+\S\+\s*{\s*'.clspat.'\%$'

    let lines = join(getline(max([ a:lnum - 10, 1]) , a:lnum-1), ' ')

    if  lines =~ pattern1 
        return indent(CppFindOccurence('namespace', a:lnum))
    elseif  lines =~ pattern2 && getline(a:lnum) =~ '^\s*{'
        return indent(CppFindOccurence('class', a:lnum))
    else
        return cindent
    endif

endfunction

function! CppFindOccurence(pattern, lnum)
    for line in range(a:lnum-1,a:lnum-10,-1)
        if getline(line) =~ a:pattern
            return line
        endif
    endfor
    return -1
endfunction
