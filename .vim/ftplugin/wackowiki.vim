
"============================================================================
"
" TWiki syntax file
"
" Language:        TWiki
" Last Change: Mon Sep 18 13:08:38 UTC 2006
" Maintainer:  Rainer Thierfelder <rainer{AT}rainers-welt{DOT}de>
" Additions:   Eric Haarbauer <ehaar{DOT}com{AT}grithix{DOT}dyndns{DOT}org>
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
"    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
"
"============================================================================
" Notes:    {{{1
"============================================================================
"
" This scripts contains options settings and functions for TWiki files
" (http://www.twiki.org).
"
" Options:
"
"   Customize the behavior of this ftplugin by setting values for the
"   following options in your .vimrc file.
"
"   g:Twiki_FoldAtHeadings
"     This variable, if set to a non-zero value, enables folding on TWiki
"     heading lines.  The fold level is defined by the number of plus-signs in
"     the heading marker.  For example, a line beginning with "---++" sets a
"     fold level of two until the next heading marker in the file.  If not
"     set, the option defaults to off.
"
"   g:Twiki_SourceHTMLSyntax
"     This Variable, if set to a non-zero value, enalbes sourcing of
"     HTLM-Syntax
"
" TODO
"   g:Twiki_Functions
"     If set, some (usefull) functions will be set
"
"============================================================================
" Initialization:    {{{1
"============================================================================
" Only do this when not done yet for this buffer
if exists("b:did_ftplugin")
  finish
endif

let b:did_ftplugin = 1

let b:undo_ftplugin = "setl com< cms< fdm< fo< foldexpr< wrap<"

" General options: {{{1

" Prevent textwidth formatting because TWiki files are wrapped on the client
" side at rendering time.
setlocal tw=0
setlocal formatoptions-=taq
setlocal commentstring=\%\%(comments)%s\%\%
setlocal infercase

" Three-space indentation is significant to TWiki
setlocal tabstop=2
setlocal shiftwidth=2
setlocal softtabstop=2
" tabs should be converted to spaces
setlocal expandtab


" Mapings : {{{1

call IMAP("***","  * ","wackowiki")

inoremap <buffer> <M-=> ==<Left>
nnoremap <buffer> <M-=> :s/^\(=*\) \?/=\1 /<Cr>:s/ \?\(=*\)$/ \1=/<Cr>
inoremap <buffer> <M-Cr> <Cr>* 

" Autoconfigure vim indentation settings

" vim: set ft=vim fdm=marker fdl=1:
