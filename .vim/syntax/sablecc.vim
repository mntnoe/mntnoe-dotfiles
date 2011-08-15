" Vim syntax file
" Language:    sablecc
" Author:      Rhodes Brown <RhodesBrown@acm.org>
" Last Change: July 9, 2004

" To install, put this file in ~/.vim/syntax/
" and add the lines
"   autocmd BufEnter *.sablecc set syntax=sablecc
"   au Syntax sablecc so $HOME/.vim/syntax/sablecc.vim
" to your ~/.vimrc file

" Remove any old syntax stuff hanging around.
syn clear
syn case match

" Names (including package names)
syn match sccId "[a-zA-Z_][0-9a-zA-Z._]*"
hi  link  sccId NONE

" File Sections
syn keyword sccSection Package States Helpers Tokens Ignored Productions Abstract Syntax Tree
hi  link    sccSection Keyword

" Pattern Operators
syn match sccPattern "|"
syn match sccPattern "\*"
syn match sccPattern "+"
syn match sccPattern "?"
hi  link  sccPattern Structure

" Transform Rules
syn keyword sccTransform New Null
hi  link    sccTransform Keyword

" State Transition/Transform
syn match sccAction "->"
hi  link  sccAction Operator

" List Operators for Transforms
syn match sccList "\["
syn match sccList "\]"
hi  link  sccList Operator

" Token/Production Specifiers
syn match sccQualifier "T\."
syn match sccQualifier "P\."
hi  link  sccQualifier Operator

" Character Set Operators
syn match sccCharSet "\["
syn match sccCharSet "\]"
syn match sccCharSet "+"
syn match sccCharSet "-"
hi  link  sccCharSet Structure

" State Specifiers, CST->AST Transforms
syn region sccStateRule     start="{" end="}" contains=sccAction
syn region sccTransformType start="{" end="}" contains=sccPattern,sccAction,sccLabel,sccQualifier
syn region sccTransformRule start="{" end="}" contains=sccAction,sccTransform,sccList

" Rule Alternative Labels
syn match sccAlternative "{[a-z][a-z_0-9]*}"
hi  link  sccAlternative Identifier

" Plain, {State} Token, Rule {Transform}
syn match sccRuleLhs "[a-z][a-z_0-9]*\s*="           contains=sccAssign
syn match sccRuleLhs "{[^}]*}\s*[a-z][a-z_0-9]*\s*=" contains=sccAssign,sccStateRule
syn match sccRuleLhs "[a-z][a-z_0-9]*\s*{[^}]*}\s*=" contains=sccAssign,sccTransformType
hi  link  sccRuleLhs Identifier

" Hack to uncolour '=' in sccRuleLhs
syn match sccAssign "="
hi  link  sccAssign NONE

" Rule Element Labels
syn match sccLabel "\[[a-z][a-z_0-9]*\]:"
hi  link  sccLabel Tag

" Numbers, Characters, Strings
syn match Number "[0-9][0-9]*"
syn match Number "0[x\|X][0-9a-fA-F]*"
syn match String "'[^'\n\r][^'\n\r]*'"

" Single and Multi-line Comments
syn region Comment oneline start="//" end="$"
syn region Comment start="/\*" skip="/\*" end="\*/"

if !exists("did_sablecc_syntax_inits")
  let did_sablecc_syntax_inits = 1
  hi Comment ctermfg=Blue
endif

let b:current_syntax = "sablecc"
