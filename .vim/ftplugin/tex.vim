" File:     tex.vim
" Author:   Mads N Noe <mntnoe (@) gmail.com>
" Modified: 2011-05-22

" For completion of labels
 setlocal iskeyword+=:,-

setlocal wildignore+=*.aux,*.log,*.toc,*.dvi,*.pdf
setlocal path=.,$HOME/.local/share/latex

setlocal textwidth=80
setlocal shiftwidth=2 softtabstop=2
setlocal infercase

command! GPre .-1r ~/docs/gsvn/preamble.tex
command! EPre tabe ~/.local/share/latex/preamble.tex

"set errorformat=%-G%.%#Underfull%.%#,%-G%.%#Overfull%.%#,%-G%.%#specifier\ changed\ to%.%#,%-G%.%#You\ have\ requested%.%#,%-G%.%#Missing\ number%\,\ treated\ as\ zero.%.%#,%-G%.%#There\ were\ undefined\ references%.%#,%-G%.%#Citation\ %.%#\ undefined%.%#,%E!\ LaTeX\ %trror:\ %m,%E!\ %m,%+WLaTeX\ %.%#Warning:\ %.%#line\ %l%.%#,%+W%.%#\ at\ lines\ %l--%*\d,%+WLaTeX\ %.%#Warning:\ %m,%-Cl.%l\ %m,%-Cl.%l\ ,%-C\ \ %m,%-C%.%#-%.%#,%-C%.%#[]%.%#,%-C[]%.%#,%-C%.%#%[{}\]%.%#,%-C<%.%#>%m,%-GSee\ the\ LaTeX%m,%-GType\ \ H\ <return>%m,%-G\ ...%.%#,%-G%.%#\ (C)\ %.%#,%-G(see\ the\ transcript%.%#),%-G\s%#,%-O(%*[^()])%r,%-P(%f%r,%-P\ %\=(%f%r,%-P%*[^()](%f%r,%-P(%f%*[^()],%-P[%\d%[^()]%#(%f%r,%-P%*[^()],%-Q)%r,%-Q%*[^()])%r,%-Q[%\d%*[^()])%r,%-Q%*[^()],%-G%.%#
"
"let ign_patterns = [ 'Underfull', 'Overfull', 'specifier changed to', 'You have requested', 'Missing number, treated as zero.', 'There were undefined references', 'Citation %.%# undefined', 'Float too large' ]
"
"for ign_pattern in ign_patterns
"    let ign_pattern = escape(substitute(ign_pattern, '[\,]', '%\\\\&', 'g'), ' ')
"    exe 'setlocal efm+=%-G%.%#'.ign_pattern.'%.%#'
"endfor

" Force xdvi to redraw.
let b:OmniMake_cmds = ["call Tex_RunLaTeX()", "silent !killall -USR1 xdvi.bin"]

nnoremap <buffer> <M-,> :call Tex_ForwardSearchLaTeX()<Cr>

let s:tex_sections = '^\\\%(\%(sub\)*section\*\{0,1}{\\|documentclass[\)'
exec "nnoremap <buffer> [ ?". s:tex_sections."\<Cr>"
exec "nnoremap <buffer> ] /". s:tex_sections."\<Cr>"

nnoremap <buffer> <M-\> viw<Esc>`>a<C-v>}<Esc>`<i\{<Left>
vnoremap <buffer> <M-\> <Esc>`>a<C-v>}<Esc>`<i\{<Left>
nnoremap <buffer> <M-'> vi{<Esc>`><Right>x`<dF\
nnoremap <buffer> <LocalLeader>/ /\\\(sub\)*section\*\?{.*\zs

nnoremap <buffer> <M-S> "xdi{vF\c<C-r>x
nnoremap <buffer> <M-R> "xdi[vF\c<C-r>x

inoremap <buffer> <M-Cr> <End> \\<Cr>

" should not be local to buffer
nnoremap <LocalLeader>lr :read !grep -ho "\\\\label{.*}"
nnoremap <LocalLeader>lp :TTarget pdf<Cr>
nnoremap <LocalLeader>ld :TTarget dvi<Cr>

nnoremap <buffer> <M-0> :s/\%(\\\%(sub\)*section{\)\?\([^}]*\)}\?/\1/<Cr>
nnoremap <buffer> <M-1> :s/\%(\\\%(sub\)*section{\)\?\([^}]*\)}\?/\\section{\1}/<Cr>
nnoremap <buffer> <M-2> :s/\%(\\\%(sub\)*section{\)\?\([^}]*\)}\?/\\subsection{\1}/<Cr>
nnoremap <buffer> <M-3> :s/\%(\\\%(sub\)*section{\)\?\([^}]*\)}\?/\\subsubsection{\1}/<Cr>

function! GenerateTags()
        execute "!cd %:p:h ; ~/.vim/extern/ltags %:p"
endfunction

command! Gentags call GenerateTags()

setlocal runtimepath^=~/.vim/help/latex


call IMAP("EAL", "\\begin{align*}\<cr><++>\<cr>\\end{align*}", "tex")
call IMAP("EIT", "\\begin{itemize}\<cr>\\item <++>\<cr>\\end{itemize}", "tex")
call IMAP("EVE", "\\begin{verbatim}\<cr><++>\<cr>\\end{verbatim}", "tex")
call IMAP("Sse", "\\section*{<++>}", "tex")
call IMAP("Sss", "\\subsection*{<++>}", "tex")
call IMAP("Ss2", "\\subsubsection*{<++>}", "tex")
call IMAP("EDR", "\\begin{draft}\<cr>\<Esc>i<++>\<cr>\\end{draft}", "tex")
call IMAP("ELS", "\\begin{lstlisting}\<cr>\<Esc>i<++>\<cr>\\end{lstlisting}", "tex")

call IMAP("==", "& =", "tex")
call IMAP("qq", "$<++>$", "tex")

call matchadd("Heading", '\\\%(\%(sub\)\{,2}section\>\|part\>\|chapter\>\)\*\?{\zs.\{-}\ze}')

if has('gui_running')
    call matchadd("Italic", '\\textit{\zs.\{-}\ze}')
    call matchadd("FixedWidth", '\\texttt{\zs.\{-}\ze}')
endif

call matchadd("ErrorMsg", '\\todo{\zs.\{-}\ze}')

