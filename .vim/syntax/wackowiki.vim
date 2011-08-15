"============================================================================
"
" WackoWiki syntax file
"
" Based on:
"
" Language:    TWiki
" Last Change: Wed Nov 22 16:14:41 UTC 2006
" Maintainer:  Rainer Thierfelder <rainer{AT}rainers-welt{DOT}de>
" Additions:   Eric Haarbauer <ehaar{DOT}com{AT}grithix{DOT}dyndns{DOT}org>
"              Antonio Terceiro <terceiro{AT}users{DOT}sourceforge{DOT}net>
" License:     GPL (http://www.gnu.org/licenses/gpl.txt)
"    Copyright (C) 2004-2006  Rainer Thierfelder
"
"    This program is free software; you can redistribute it and/or modify
"    it under the terms of the GNU General Public License as published by
"    the Free Software Foundation; either version 2 of the License, or
"    (at your option) any later version.
"
"    This program is distributed in the hope that it will be useful,
"    but WITHOUT ANY WARRANTY; without even the implied warranty of
"    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
"    GNU General Public License for more details.
"
"    You should have received a copy of the GNU General Public License
"    along with this program; if not, write to the Free Software
"    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA
"
"============================================================================
"
" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if !exists("main_syntax")
  if version < 600
    syntax clear
  elseif exists("b:current_syntax")
    finish
  endif
  let main_syntax = 'wackowiki'
endif

if exists("g:Wackowiki_SourceHTMLSyntax") && g:Wackowiki_SourceHTMLSyntax != 0
  source $VIMRUNTIME/syntax/html.vim 
endif

" Don't use standard HiLink, it will not work with included syntax files
if version < 508
  command! -nargs=+ WackowikiHiLink   highlight link <args>
  command! -nargs=+ WackowikiSynColor highlight <args>
else
  command! -nargs=+ WackowikiHiLink   highlight default link <args>
  command! -nargs=+ WackowikiSynColor highlight default <args>
endif

"============================================================================
" Group Definitions:    {{{1
"============================================================================

syntax match wackowikiSeparator    "^----\+"
syntax match wackowikiBulletedList "^\( \)\+\*\ze "
syntax match wackowikiOrderedList  "^\( \)\+1\ze "

syntax match wackowikiSimpleVariable "\([^!]\|^\)\zs%\w\+%"
syntax match wackowikiVariableParam contained "[a-z0-9]*="
syntax region wackowikiVariableValue start="\"" skip="\\\"" end="\"" contains=wackowikiSimpleVariable,wackowikiVariable
syntax region wackowikiVariable start="\([^!]\|^\)\zs%\w\+{" end="}%"
    \ contains=wackowikiVariableParam,wackowikiVariableValue,WackowikiHiLink,wackowikiSimpleVariable,wackowikiVariable
syntax match wackowikiTag      "<\w\+>"

syntax match wackowikiDelimiter "|"

syntax region wackowikiComment  start="<!--" end="-->"
syntax region wackowikiVerbatim matchgroup=wackowikiTag
    \ start="<verbatim>" end="</verbatim>"
syntax region wackowikiPre matchgroup=wackowikiTag contains=wackowikiVariable,wackowikiSimpleVariable
    \ start="<pre>" end="</pre>"

syntax region wackowikiHeading matchgroup=wackowikiHeadingMarker oneline
    \ start="^---+\+" end="$"

"let s:wikiWord = '\(\w\+\.\)\?\u[a-z0-9]\+\(\u[a-z0-9]\+\)\+'
let s:wikiWord = '\u\+[a-z0-9]\+\(\u\+[a-z0-9]\+\)\+'

execute 'syntax match wackowikiAnchor +^#'.s:wikiWord.'\ze\(\>\|_\)+'
execute 'syntax match wackowikiWord +\(\s\|^\)\zs\(\u\l\+\.\)\='.s:wikiWord.'\(#'.s:wikiWord.'\)\=\ze\(\>\|_\)+'
" Regex guide:                   ^pre        ^web name       ^wikiword  ^anchor               ^ post

" Links: {{{2
syntax region wackowikiLink matchgroup=wackowikiLinkMarker
    \ start="\( \|^\)\zs\[\[" end="\]\]\ze\([,. ?):-]\|$\)"
    \ contains=wackowikiForcedLink,wackowikiLinkRef keepend

execute 'syntax match wackowikiForcedLink +[ A-Za-z0-9]\+\(#'.s:wikiWord.'\)\=+ contained'

syntax match wackowikiLinkRef    ".\{-}\ze\]\["
    \ contained contains=wackowikiLinkMarker nextgroup=wackowikiLinkLabel
syntax match wackowikiLinkLabel  ".\{-}\ze\]\]"   contained contains=wackowikiLinkMarker
syntax match wackowikiLinkMarker "\]\["           contained

" Emphasis:  {{{2
function! s:WackowikiCreateEmphasis(token, name)
    execute 'syntax region wackowiki'.a:name.
           \' oneline start=+\(^\|[ ]\)\zs'.a:token.
           \'+ end=+'.a:token.'\ze\([,. ?):-]\|$\)+'
endfunction

call s:WackowikiCreateEmphasis('==',  'BoldFixed')
call s:WackowikiCreateEmphasis('===', 'BoldFixed')
call s:WackowikiCreateEmphasis('====','BoldFixed')
call s:WackowikiCreateEmphasis('!!',  'Bold')
call s:WackowikiCreateEmphasis('##',  'Bold')
call s:WackowikiCreateEmphasis('%%',  'Bold')
call s:WackowikiCreateEmphasis('%%',  'Bold')
call s:WackowikiCreateEmphasis('\*\*','Bold')
call s:WackowikiCreateEmphasis('//',  'Bold')
call s:WackowikiCreateEmphasis('__',  'Fixed')
call s:WackowikiCreateEmphasis('--',  'Fixed')

"============================================================================
" Group Linking:    {{{1
"============================================================================

WackowikiHiLink wackowikiHeading       String
WackowikiHiLink wackowikiHeadingMarker Operator
WackowikiHiLink wackowikiVariable      PreProc
WackowikiHiLink wackowikiVariableParam Type
WackowikiHiLink wackowikiVariableValue String
WackowikiHiLink wackowikiTag           PreProc
WackowikiHiLink wackowikiComment       Comment
WackowikiHiLink wackowikiWord          Tag
WackowikiHiLink wackowikiAnchor        PreProc
WackowikiHiLink wackowikiVerbatim      Constant
WackowikiHiLink wackowikiPre           Constant
WackowikiHiLink wackowikiBulletedList  Operator
WackowikiHiLink wackowikiOrderedList   Operator

WackowikiHiLink wackowikiDelimiter     Operator

" Links
WackowikiSynColor wackowikiLinkMarker term=bold cterm=bold gui=bold
WackowikiHiLink   wackowikiForcedLink Tag
WackowikiHiLink   wackowikiLinkRef    Tag
WackowikiHiLink   wackowikiLinkLabel  Identifier

" Emphasis
WackowikiSynColor wackowikiFixed      term=underline cterm=underline gui=underline
WackowikiSynColor wackowikiBoldFixed  term=bold,underline cterm=bold,underline gui=bold,underline
WackowikiSynColor wackowikiItalic     term=italic cterm=italic gui=italic
WackowikiSynColor wackowikiBoldItalic term=bold,italic cterm=bold,italic gui=bold,italic
WackowikiSynColor wackowikiBold       term=bold cterm=bold gui=bold

"============================================================================
" Clean Up:    {{{1
"============================================================================

delcommand WackowikiHiLink
delcommand WackowikiSynColor

if main_syntax == 'wackowiki'
  unlet main_syntax
endif

let b:current_syntax = "wackowiki"

" vim:fdm=marker
