" File:     mntnoe-light.vim
" Author:   Mads N Noe <mntnoe (@) gmail.com>
" Modified: 2010-10-11

" The mntnoe-light color theme for Vim. Originated from Delek.

hi clear

let colors_name = "mntnoe-light"

" Normal should come first
hi Normal     guifg=Black  guibg=White
hi Cursor     guifg=bg     guibg=fg
hi lCursor    guifg=NONE   guibg=Cyan

" Note: we never set 'term' because the defaults for B&W terminals are OK
hi DiffAdd    ctermbg=LightBlue    guibg=LightBlue
hi DiffChange ctermbg=LightMagenta guibg=LightMagenta
hi DiffDelete ctermfg=Blue	   ctermbg=LightCyan gui=bold guifg=Blue guibg=LightCyan
hi DiffText   ctermbg=Red	   cterm=bold gui=bold guibg=Red
hi Directory  ctermfg=DarkBlue	   guifg=Blue
hi ErrorMsg   ctermfg=Red	   ctermbg=NONE  guifg=Red	    guifg=NONE
hi FoldColumn ctermfg=DarkBlue	   ctermbg=Grey     guibg=lightgray	    guifg=black
hi Folded     ctermbg=Grey	   ctermfg=DarkBlue guibg=LightGrey guifg=black
hi IncSearch  cterm=reverse	   gui=reverse
hi LineNr     ctermfg=Brown	   guifg=Brown
hi ModeMsg    cterm=bold	   gui=bold
hi MoreMsg    ctermfg=DarkGreen    gui=bold guifg=SeaGreen
hi NonText    ctermfg=Blue	   gui=bold guifg=gray guibg=white
hi Pmenu      guibg=LightBlue
hi PmenuSel   ctermfg=White	   ctermbg=DarkBlue  guifg=White  guibg=DarkBlue
hi Question   ctermfg=DarkGreen    gui=bold guifg=SeaGreen
hi Search     ctermfg=NONE	   ctermbg=Yellow guibg=Yellow guifg=NONE
hi SpecialKey ctermfg=DarkBlue	   guifg=Blue
hi StatusLine cterm=bold	   ctermbg=236 ctermfg=251 guibg=black guifg=lightgray
hi StatusLineNC	cterm=bold	   ctermbg=DarkBlue ctermfg=black  guibg=gold guifg=blue
hi TabLine     term=NONE cterm=underline ctermbg=241 ctermfg=251
hi TabLineFill term=NONE cterm=underline ctermbg=241 ctermfg=251
hi TabLineSel  term=NONE cterm=bold ctermbg=245 ctermfg=255
hi Title      ctermfg=DarkRed  gui=bold guifg=Magenta
hi VertSplit  cterm=reverse	   gui=reverse
hi Visual     ctermbg=LightGrey
hi VisualNOS  cterm=underline,bold gui=underline,bold
hi WarningMsg ctermfg=DarkRed	   guifg=Red
hi WildMenu   ctermfg=Black	   ctermbg=Yellow    guibg=Yellow guifg=Black

" syntax highlighting
hi Comment    cterm=NONE ctermfg=246     gui=NONE guifg=gray
hi Constant   cterm=NONE ctermfg=DarkGreen   gui=NONE guifg=green3
hi Identifier cterm=NONE ctermfg=DarkCyan    gui=NONE guifg=cyan4
hi PreProc    cterm=bold ctermfg=DarkRed gui=NONE guifg=magenta3
hi Special    cterm=NONE ctermfg=Blue    gui=NONE guifg=deeppink
hi Statement  cterm=bold ctermfg=Blue	     gui=bold guifg=blue
hi Type	      cterm=NONE ctermfg=Blue	     gui=bold guifg=blue

highlight SpellBad   ctermfg=9  ctermbg=NONE cterm=underline 
highlight SpellCap   ctermfg=12 ctermbg=NONE cterm=underline 
highlight SpellRare  ctermfg=13 ctermbg=NONE cterm=underline 
highlight SpellLocal ctermfg=14 ctermbg=NONE cterm=underline 
highlight MatchParen ctermfg=7  ctermbg=249  cterm=none

hi Heading term=underline,bold cterm=underline,bold ctermfg=232 gui=bold
hi Italic  gui=italic
hi FixedWidth gui=italic


" vim: sw=2
