" File:     java.vim
" Author:   Mads N Noe <mntnoe (@) gmail.com>
" Modified: 2009-12-03
"
" Compilation is done with ant.

inoreabbr <buffer> syso System.out.println
inoreabbr <buffer> syse System.err.println

setlocal runtimepath^=~/.local/share/vim/java

if !exists("b:java_cp")   | let b:java_cp = "."
endif

if !exists("b:java_exec") | let b:java_exec = "%"
endif

" Generally, <M-m> compiles and <M-,> executes.
nnoremap <buffer> <M-,> :!java -cp <C-r>=expand(b:java_cp)<Cr> <C-r>=substitute(expand(b:java_exec),'\.java$','','')<Cr><Cr>

call IMAP(":CL", "public class \<C-r>\<C-o>=expand(\"%:t:r\")\n {\npublic \<C-r>\<C-o>=expand(\"%:t:r\")\n() {<++>\n}\n}", "java")
call IMAP(":PUB", "public <+type+> <+name+>(<+args+>) {\n<++>\n}", "java")
call IMAP(":PRI", "private <+type+> <+name+>(<+args+>) {\n<++>\n}", "java")
call IMAP(":MN", "public static void main(String [] args) {\n<++>\n}", "java")
call IMAP(":SO", "System.out.println(<++>);", "java")
