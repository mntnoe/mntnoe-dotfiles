" File:     mntnoe.vim
" Author:   Mads N Noe <mntnoe (@) gmail.com>
" Modified: 2010-05-27

" The mntnoe color theme for Vim. Originated from Pablo, but with improved
" readability. Note that comments are less bright than non-comments, making
" the code more visible. Also check my .Xdefaults file for xterm colors.

hi clear
set background=dark
if exists("syntax_on")
  syntax reset
endif
let g:colors_name = "mntnoe"

highlight Comment    ctermfg=8                          guifg=#868686
highlight Constant   ctermfg=14             cterm=none  guifg=#54ffff               gui=none
highlight Identifier ctermfg=6                          guifg=#18b2b2
highlight Statement  ctermfg=11             cterm=none  guifg=#ffff54               gui=none
highlight PreProc    ctermfg=10                         guifg=#24ed24
highlight Type       ctermfg=2                          guifg=#18b218
highlight Special    ctermfg=12                         guifg=#3484ef
highlight Error                 ctermbg=9                             guibg=#ee5454
highlight ErrorMsg   ctermfg=9  ctermbg=NONE            guifg=#fc5454 guibg=NONE
highlight Todo       ctermfg=4  ctermbg=3               guifg=#002075 guibg=#b26818
highlight Directory  ctermfg=2                          guifg=#18b218
highlight StatusLine ctermfg=0  ctermbg=7   cterm=none  guifg=#182222 guibg=#c0c0c0 gui=none
highlight Normal                                        guifg=#c0c0c0 guibg=#182227
highlight Search                ctermbg=3                             guibg=#b26818
highlight NonText    ctermfg=12             cterm=bold  guifg=#3484ef               gui=bold
highlight Visual                ctermbg=239 term=reverse
hi TabLine     term=NONE cterm=underline ctermbg=237 ctermfg=grey
hi TabLineFill term=NONE cterm=underline ctermbg=237 ctermfg=grey
hi TabLineSel  term=NONE cterm=NONE ctermbg=245 ctermfg=white
highlight Folded     ctermfg=11 ctermbg=238             guifg=#ffff54 guibg=#464646
highlight FoldColumn ctermfg=11 ctermbg=238             guifg=#ffff54 guibg=#464646
highlight SignColumn ctermfg=11 ctermbg=238             guifg=#ffff54 guibg=#464646
highlight Question   ctermfg=10                         guifg=#ffff54               gui=none
highlight MoreMsg    ctermfg=10                         guifg=#ffff54               gui=bold
highlight SpellBad   ctermfg=9  ctermbg=NONE cterm=underline 
highlight SpellCap   ctermfg=12 ctermbg=NONE cterm=underline 
highlight SpellRare  ctermfg=13 ctermbg=NONE cterm=underline 
highlight SpellLocal ctermfg=14 ctermbg=NONE cterm=underline 
highlight MatchParen ctermfg=7  ctermbg=237  cterm=none

hi Heading term=underline,bold cterm=underline,bold ctermfg=15 

