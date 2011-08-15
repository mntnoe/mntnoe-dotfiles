" File:     .vimrc
" Author:   Mads N Noe <mail (@) madsnoe.dk>
" License:  as-is
" Modified: 2011-03-12
"
" {{{1 {{{2
" 
" This .vimrc is usable with both the Colemak and Qwerty layout. However, I
" suggest that you swap Y and J in Colemak. This makes the navigation buttons
" arguably just as easy to reach (depending on the angle of your right hand).
"
" Also, I use a couple of translations in my .Xdefaults. <M- > and <S- > does
" not work in xterm, so I have mapped them to some bindings I wouldn't
" possibly use. If you can't see the special characters, I have created a
" comment where they occur.
"
" You get a good overview of the file by setting fdm=marker fdc=4 fdl=1
" (or use the modeline in the bottom with securemodelines).
" 
" As of writing, I use these external plugins. I had to modify some of them in
" the source (mainly to solve conflicts with hard-coded bindings), but I do
" not remember where, unfortunately.
"
"   * AlignPlugin
"   * NERD_commenter
"   * a
"   * imaps
"   * matchit
"   * project
"   * rhs
"   * securemodelines
"   * surround
"   * taglist
"   * timestamp
"   * vcscommand
"   * vcssvn
"
" File plugins:
"
"   * haskellmode
"   * latexsuite
"   * a number of smaller burrowed snippets
" }}}2 }}}1

" GENERAL OPTIONS {{{1
" ---------------

" FILE LOADING {{{
filetype plugin indent on
syntax on

set encoding=latin1

" I use ISO-8859-1 as default. Unfortunately Vim will not detect Unicode
" automatically such a system, so you have do to it manually.
if !has('win32')
    set fileencodings=ucs-bom,default
endif

" use securemodelines instead
set nomodeline

set sessionoptions=buffers,curdir,folds,help,tabpages,winsize
" }}}

" MISC BEHAVIOR {{{

" The header and footer command uses the commentstring, so remove the default
" C style string when no file type is active.
set commentstring=\%s

set timeoutlen=1000
set ttimeoutlen=20
set updatetime=1000

set noequalalways
set switchbuf=usetab

set whichwrap=b,s,<,>,[,]
set nostartofline
set nojoinspaces
set spellcapcheck=

set splitright splitbelow

set backspace=indent,eol,start	" more powerful backspacing
set history=50		" keep 50 lines of command line history

" }}}

" TITLE {{{
if has("gui_running")
    set titlestring=gvim\:\ %t
else
    set titlestring=vim\:\ %t
endif
" }}}

" TERMINAL {{{
if !has("gui_running")
    set noerrorbells visualbell t_vb=
endif
if &term == "screen"
    " Make colors work in GNU Screen.
    set t_Co=256
endif

if !has("gui_running")
    set <S-F1>=[1;2P
    set <S-F2>=[1;2Q
    set <S-F3>=[1;2R
    set <S-F4>=[1;2S
    set <S-F5>=[15;2~
    set <S-F6>=[17;2~
    set <S-F7>=[18;2~
    set <S-F8>=[19;2~
    set <S-F9>=[20;2~
    set <S-F10>=[21;2~
    set <S-F11>=[23;2~
    set <S-F12>=[24;2~
    set <S-Left>=[1;2D
    set <S-Right>=[1;2C
    set <S-Up>=[1;2A
    set <S-Down>=[1;2B
    set <S-Home>=[1;2H
    set <S-End>=[1;2F
    set <C-Home>=[1;5H
    set <C-End>=[1;5F
    set <S-Insert>=[2;2~
    set <S-Delete>=[3;2~
    set <F13>=[27;2;13~
endif
" }}}

" COMPLETION {{{
set wildmenu
set wildmode=longest:full,full
set completeopt=menuone,longest,preview
" }}}

" SEARCH {{{
set incsearch
set nohlsearch
set ignorecase
"set gdefault
" }}}

" BRACKETS {{{
set showmatch
set matchtime=3
" }}}

" INDENTATION {{{
set shiftround
set expandtab
set tabstop=8
set shiftwidth=4
set softtabstop=4
" }}}

" LEADER {{{
let mapleader=','
let maplocalleader=','
" }}}

" PATHS {{{
set backupdir=~/.vim/backup

" Ensure the ~/.vim is the first in rtp.
set runtimepath-=~/.vim
set runtimepath^=~/.vim/latexsuite
set runtimepath^=~/.vim
" }}}

" GUI {{{
if has("gui_running")
    set mouse=a
    set guioptions=agicme
    set winaltkeys=no
    set guifont=Consolas\ 10
    colorscheme mntnoe-light
else
    set mouse=ni 

    if $XTERM_THEME=="light"
        colorscheme mntnoe-light
    else
        colorscheme mntnoe
    endif
endif
set confirm
set showtabline=2
set shortmess=filmnrwxtToOI
" }}}

" TABLINE{{{

function! MyTabLine()
    let s = ''
    for i in range(tabpagenr('$'))
        " select the highlighting
        if i + 1 == tabpagenr()
            let s .= '%#TabLineSel#'
        else
            let s .= '%#TabLine#'
        endif

        " set the tab page number (for mouse clicks)
        let s .= '%' . (i + 1) . 'T'

        " the label is made by MyTabLabel()
        let s .= ' %{MyTabLabel(' . (i + 1) . ')} '
    endfor

    " after the last tab fill with TabLineFill and reset tab page nr
    let s .= '%#TabLineFill#%T'

    return s
endfunction

function! MyTabLabel(n)
    let buflist = tabpagebuflist(a:n)
    let winnr = tabpagewinnr(a:n)
    let numtabs = tabpagenr('$')
    " account for space padding between tabs, and the "close" button
    let maxlen = ( &columns - ( numtabs * 2 ) ) / numtabs
    let tablabel = bufname(buflist[winnr - 1])

    if tablabel == ""
        if &buftype=='quickfix'
            let tablabel = '[Quickfix List]'
        else
            let tablabel = '[No Name]'
        endif
    endif
    while strlen( tablabel ) < 4
        let tablabel = tablabel . " "
    endwhile
    let tablabel = fnamemodify( tablabel, ':t' )

    "  Append the number of windows in the tab page
    let wincount = tabpagewinnr(a:n, '$')
    if wincount > 1
        let tablabel = '[' . wincount . '] ' . tablabel
    endif

    " Add '+' if one of the buffers in the tab page is modified
    for bufnr in buflist
        if getbufvar(bufnr, "&modified")
            let tablabel = "+ " . tablabel
            break
        endif
    endfor 

    let tablabel = strpart( tablabel, 0, maxlen )
    return tablabel
endfunction

set tabline=%!MyTabLine()
" }}}

" STATUSLINE {{{
set laststatus=2

" Fields: fileencoding, textwidth, rhs position, flags (list, paste,
" formatoptions) and spell
" StatusLine() {{{
function! StatusLine()
    let buf_add = ""
    if exists("b:statusline_add")
        let buf_add = b:statusline_add
    endif

    let fenc = ""
    if &fenc != &enc && &fenc != ""
        let fenc = " [".&fenc."]"
    endif

    let tw = ""
    if &tw != 0              | let tw = " [w".&tw."]"           | endif
                                                                
    let fdm = ""                                                
    if &fdm != "manual"      | let fdm .= " [f".&fdl."]"        | endif
                                                                
    let rhs = ""
    if exists("b:rhs_fixed") | let rhs .= " [=".b:rhs_fixed."]" | endif
                                                                
    let flags = ""                                              
    if match(&fo, "r") != -1 | let flags .= "c"                 | endif
    if &list == 1            | let flags .= "l"                 | endif
    if &paste == 1           | let flags .= "p"                 | endif
    if flags != ""           | let flags = " [".flags."]"       | endif

    let cmt = ""
    if exists("b:left") && b:left != ""
        let cmt = " [".b:left
        if exists("b:right") && b:right != "" | let cmt .= ",".b:right."]"
        else                                  | let cmt .= "]"
        endif
        let cmt = substitute(cmt, "%", "%%", "g")
    endif

    let spell = ""                                                
    if &spell == 1 && &spelllang != ""
        let spell = " [".&spelllang."]"
    endif
                                                                
    return '%<%f%a %h%w%m%r%= %Y'.buf_add.fenc.tw.fdm.rhs.flags.cmt.spell.' %l/%L [%c%V]'
endfunction
" }}}
set statusline=%!StatusLine()
" }}}

" AUTOCOMMANDS {{{
if !exists("autocommands_loaded")
    let autocommands_loaded = 1
    autocmd FileType  *         set formatoptions-=r
    autocmd FileType  *         set formatoptions-=o
    autocmd BufEnter  *.hs      compiler ghc
    autocmd BufRead   catalog   setlocal ft=xml
    autocmd BufRead   *.module  setlocal ft=php
    autocmd BufRead   *.todo    setlocal ft=todo
    autocmd BufRead   *.txt     setlocal ft=text
    autocmd BufRead   *.sablecc setlocal ft=sablecc
    autocmd BufRead   *.clj     setlocal ft=clojure
    autocmd BufRead   *.msg     setlocal ft=mail
    autocmd BufRead   mail.google.com.* setlocal ft=mail
    autocmd BufRead   dekspsys.ingemann.de.* setlocal ft=twiki
    autocmd BufWrite  /home/mntnoe/.xmonad/lib/* silent !touch ~/.xmonad/xmonad.hs
    autocmd BufEnter  /home/mntnoe/Work/CG/project-q3/* let g:OmniMake_cmds = ["make -C ../optimized", "cwindow"]
    autocmd BufEnter  /home/mntnoe/Work/CG/project-q3/* let g:OmniRun_cmd   = "../optimized/src/powerfish"

    function! IsTransientWindow()
        let bn = bufname("")
        return bn == expand("$HOME/.vimprojects") || bn == "__Tag_List__" || &previewwindow || &ft == "qf"
    endfunction

    function! CloseIfTransientWindow()
        if IsTransientWindow()
            hide
        endif
    endfunction
    autocmd TabLeave  *         windo call CloseIfTransientWindow()

    " always highlight TODO.
    autocmd BufNewFile,BufRead,FileType * match Todo "TODO"
endif
" }}}
" }}}1

" MAPPINGS {{{1
" --------

" COMMANDS {{{
command! SO source ~/.vimrc | let &ft=&ft
command! Utf8 e ++enc=utf-8

" You might need to run WinDef in .gvimrc too.
if has("gui_running") 
    command! WinDef set lines=32 columns=120
    if !exists("isset_WinDef") | WinDef
    endif
    let isset_WinDef = 1
endif
" }}}

" EDITING {{{
nnoremap <M-p> gwip
xnoremap <M-p> gq
inoremap <M-p> <C-\><C-o>gwip

inoremap <M-s> <C-o>~
noremap  <M-s> ~
nnoremap  <M-a> viwo<Esc>g~li

" inoremap <F8>   <C-\><C-o>mm<S-Right><C-o>b<C-o>g~iw<C-o>`m
" inoremap <S-F8> <C-\><C-o>mm<S-Right><C-o>b<C-o>gUiw<C-o>`m

" To fix typo in operator pending mode.
nnoremap <M-T> guiwi

" Only use registers when I actually want to.
xnoremap <Bs> "_d
nnoremap <Bs> "_d
nnoremap <Bs>: "_D
nnoremap <Bs><Bs> "_dd

nnoremap x "_x
xnoremap x "_x
nnoremap c "_c
xnoremap c "_c
nnoremap s "_s

" Delegate <S- > binding.
cnoremap ú <Space>
noremap  ú <Space>
inoremap ú <Space>
" }}}

" INSERT/APPEND {{{
" I use these so often that I want at single stroke binding for them. the t/T
" command not that useful in normal mode, and n/N is remapped as it has a very
" good position in Colemak.
nnoremap n A
nnoremap t I
nnoremap N Vip<Esc>`>a
nnoremap T Vip<Esc>`<i
" }}}

" VISUAL {{{
" The default behavior in visual mode do not make sense. I already use / and ?.
xnoremap * y/<C-r><C-o>"<Cr>v//e<Cr>
xnoremap # y?<C-r><C-o>"<Cr>v//e<Cr>

" Make insert/add work also in visual line mode like in visual block mode.
xnoremap <silent> <expr> I (mode() =~# "[V]" ? "\<C-v>0o$I" : "I")
xnoremap <silent> <expr> t (mode() =~# "[V]" ? "\<C-v>0o$I" : "I")
xnoremap <silent> <expr> A (mode() =~# "[V]" ? "\<C-v>0o$A" : "A")
xnoremap <silent> <expr> n (mode() =~# "[V]" ? "\<C-v>0o$A" : "A")

noremap q <C-v>
" }}}

" RECORDING {{{
" One touch recording. I have never used more than one register at a time...
if !exists("g:recording")| let g:recording = 0
endif
nnoremap <silent> <expr> <F3> g:recording ? "q:let g:recording=0\<Cr>" : ":let g:recording=1\<Cr>qq"
xnoremap <silent> <expr> <F3> g:recording ? "q:\<C-u>let g:recording=0\<Cr>gv" : ":\<C-u>let g:recording=1\<Cr>gvqq"
noremap <F4> @q
" }}}

" MISC {{{
" This one I'm proud of :-) Much easier to hit than colon..
noremap <space> :

" Make Ê, ¯ and Â work in 8-bit terminals (which work better with <M-...>
" bindings).
noremap <C-v>  <Nop>
noremap <C-v>Ê <Nop>
noremap <C-v>¯ <Nop>
noremap <C-v>Â <Nop>

nnoremap <Leader>h yypVr
nnoremap <Leader>a ggVG

" undo
nnoremap U <C-r>
inoremap <CR> <C-g>u<CR>

nnoremap ! :%!
xnoremap ! :!
nnoremap @ :vsplit<Cr>

nnoremap <M-1> :only<Cr>
nnoremap <M-2> :split<Cr>
nnoremap <M-3> :vsplit<Cr>
nnoremap <M-0> :q<Cr>
" }}}

" SPELLING {{{
nnoremap <silent> <M-=> z=1<Cr><Cr>
nnoremap          <M--> ]s
nnoremap          <M-_> [s
" }}}

" COMMAND MODE {{{

" Quick access to often used Ex commands. My xterm config remaps <C-M-7> to
" <M- >.
map      †      <Space>†
cnoremap †      <Nop>

cnoremap †<Tab> <C-u>e <C-d>
cnoremap †<M-e> <C-u>e <C-d>
cnoremap †<M-t> <C-u>tabe <C-d>

cnoremap †<M-f> <C-r>=expand('%:p')<Cr>
cnoremap †<M-p> <C-r>=expand('%:p:h').'/'<Cr>
cnoremap †<M-o> <C-u>cd<Space><C-r>=expand('%:p:h').'/'<Cr><Cr>
cnoremap †<M-d> <C-u>cd<Space>

cnoremap ††    <C-u>set<Space>
cnoremap †<M-i> <C-u>set ft=

cnoremap †<M-x> <C-r>=getline(".")<Cr><Cr>
cnoremap †<M-m> <C-u>call IMAP("","","")<Left><Left><Left><Left><Left><Left><Left><Left>
 
cnoremap †<M-s> <C-u>%s///g<Left><Left><Left>
xnoremap †<M-s> :s///g<Left><Left><Left>
cnoremap †<M-r> <C-u>%s/\<<C-r><C-w>\>//g<Left><Left>
xnoremap †<M-r> y:%s/<C-r><C-o>"//g<Left><Left>
cnoremap †<M-S> <C-u>.,$s///gc<Left><Left><Left><Left>
xnoremap †<M-S> :s///gc<Left><Left><Left><Left>
cnoremap †<M-R> <C-u>.,$s/\<<C-r><C-w>\>//gc<Left><Left><Left>
xnoremap †<M-R> y:.,$s/<C-r><C-o>"//gc<Left><Left><Left>

cnoremap †<M-g> <C-u>vim //j **\|copen<Left><Left><Left><Left><Left><Left><Left><Left><Left><Left><Left>
xnoremap †<M-g> y:<C-u>vim /<C-r><C-o>"/j **\|copen<Left><Left><Left><Left><Left><Left><Left><Left><Left><Left><Left>

cnoremap †<M-h> <C-u>match Todo "\<<C-r><C-w>\>"<Cr>
xnoremap †<M-h> y:match Todo "<C-r>""

cnoremap †<M-l> <C-u>tabe ~/.vim/ftplugin/<C-r>=&ft<Cr>.vim<Cr>
cnoremap †<M-v> <C-u>e ~/.vimrc<Cr>
cnoremap †<M-z> <C-u>e ~/.zshrc<Cr>
cnoremap †<M-w> <C-u>e ~/.xmonad/xmonad.hs<Cr>

" Go left instead of home to keep cursor position.
cnoremap <M-/> \ze.*{ <Bs>{{<Left><Left><Left><Left><Left><Left><Left><Left>
cnoremap <M-.> \<\><Left><Left>

" See my .Xmodmap.
cnoremap <PageUp> <S-Left>
cnoremap <PageDown> <S-Right>

" Alternative mappings to balance load on hands
cnoremap <M-h> <Left>
cnoremap <M-l> <Right>
cnoremap <M-k> <Up>
cnoremap <M-j> <Down>

" For when I press <Cr> to fast.
cnoremap <M-Cr> <Cr>
" }}}

" INSERT MODE {{{

inoremap <M-h> <Left>
inoremap <M-l> <Right>
inoremap <M-K> <C-g>k
inoremap <M-J> <C-g>j
" tentative
inoremap <M-H> <S-Left>
inoremap <M-L> <S-Right>
" inoremap <...> <C-\><C-o>dw

" Backspace is located at qwerty P on my keyboard layout.
inoremap <C-M-h> <C-w>
cnoremap <C-M-h> <C-w>
inoremap <M-Bs> <C-w>
cnoremap <M-Bs> <C-w>


" Home (eclipse style)
nnoremap <expr> <Home> match(getline("."), '^\s*\zs') + 1 == col(".") ? "0" : "^"
inoremap <expr> <Home> match(getline("."), '^\s*\zs') + 1 == col(".") ? "\<C-o>0" : "\<C-o>^"

" }}}

" OPEN NEW LINE {{{
inoremap <M-o> <C-\><C-o>mm<C-o>o<C-o>`m
inoremap <M-O> <C-\><C-o>mm<C-o>O<C-o>`m
xnoremap <M-o> <Esc>`<O<Esc>`>o<Esc>`<<Up>v`><Down>
" }}}

" SCROLLING {{{
noremap  <M-k> <C-b>
noremap  <M-j> <C-f>
inoremap <M-k> <C-o><C-y>
inoremap <M-j> <C-o><C-e>

inoremap <C-l> <C-o>zz
" }}}

" TABS AND WINDOWS {{{
" Backspace is located at qwerty P on my keyboard layout.
nnoremap <M-l> gt
nnoremap <M-h> gT
nnoremap <M-t>   :tabnew<Cr>
nnoremap <M-r>   <C-w><C-w>

nnoremap <M-`>   :qall<Cr>

" local to Close()
function! Close_TestIsTransientWindow()
    if !IsTransientWindow()
        let g:nontransient_window_count += 1
    endif
endfunction

function! Close()
    let tab_count = tabpagenr("$")
    let window_count = winnr("$")

    let g:nontransient_window_count = 0
    let cur_window = winnr()
    windo call Close_TestIsTransientWindow()
    execute cur_window."wincmd w"

    if has('gui_running')
        if window_count > 1 && IsTransientWindow()
            quit
        elseif tab_count == 1 && window_count == 1
            enew
        elseif tab_count == 1 && g:nontransient_window_count <= 1
            only
            enew
        elseif g:nontransient_window_count <= 1
            tabclose
        else
            quit
        endif
    else
        if tab_count > 1 && g:nontransient_window_count <= 1
            tabclose
        elseif tab_count == 1 && g:nontransient_window_count <= 1
            only
            quit
        else
            quit
        endif
    endif
endfunction
nnoremap <M-q>   :call Close()<Cr>

nnoremap <M-w>   :w<Cr>
inoremap <M-w>   <C-o>:w<Cr>
" balance finger load
nnoremap <C-s>   :up<Cr>

nnoremap <M-b>   :<C-u>tab sbuf <C-d>
nnoremap <M-F>   :tab Explore<Cr>

" Open the visual selection in a new split at the top, reusing existing
" window splits.
" VisualSplit() {{{
function! <SID>VisualSplit()
    let top = getpos("'<")[1]
    let bot = getpos("'>")[1]
    let height = bot - top + 1
    normal \<Esc>
    only
    split
    execute "1 wincmd w"
    " position and size the top window
    execute "normal '<z" . height . "\<Cr>z\<Cr>"
    " return to 'main' window
    execute "normal \<C-w>2\<C-w>"
endfunction
" }}}
xnoremap <C-w><M-s> :<C-u>call <SID>VisualSplit()<Cr>

" Set window height to the length of its contents (or maximize it).
" MaxSplit() {{{
function! <SID>MaxSplit()
    let tobottom = 0
    if line("$") < winheight(".")
        let tobottom = 1
    endif
    execute "normal z" . line("$") . "\<Cr>"
    " temporary hack: make "~" lines disappear
    if tobottom == 1
        normal zb
    endif
endfunction
" }}}
nnoremap <C-w><C-m> :call <SID>MaxSplit()<Cr>

" }}}

" FOLDING {{{
nnoremap <M-u> zc
nnoremap <M-o> zo
nnoremap <M-U> zC
nnoremap <M-O> zO
" }}}

" BRACKETS {{{
" InsertBracketsHelper() {{{
function! InsertBracketsHelper() 
    if winheight(0) - winline() == 0
        exec "normal! \<C-e>"
    endif
endfunction 
" }}}
inoremap <M-}> <End><Space>{<Cr>}<Esc>:call InsertBracketsHelper()<Cr>O
inoremap <M-]> <End><Cr>{<Cr>}<Esc>:call InsertBracketsHelper()<Cr>O

inoremap <M-9> ()<Left>
inoremap <M-{> {}<Left>
inoremap <M-[> []<Left>

inoremap <M-;> <End>;<Cr>
" }}}

" OMNIMAKE {{{
" Fast and flexible way to call :make or similar.
" --- include/omit 'silent'? ---
let g:OmniMake_cmds = ["make", "cwindow"]
" OmniMake() {{{
function! OmniMake()

    " Fix focus in for instance latex-suite.
    if &previewwindow
        exec "normal \<C-w>k"
    endif
    if &filetype == "qf"
        exec "normal \<C-w>k"
    endif

    let cmds = g:OmniMake_cmds
    if exists("b:OmniMake_cmds")
        let cmds = b:OmniMake_cmds
    endif

    " autowrite writes too much...
    update
    for cmd in cmds
        exec cmd
    endfor

    " Fix focus in for instance latex-suite.
    if &previewwindow
        exec "normal \<C-w>k"
    endif
    if &filetype == "qf"
        exec "normal \<C-w>k"
    endif

    redraw!
endfunction
" }}}
nnoremap <M-m> :call OmniMake()<Cr>
inoremap <M-m> <Esc>:call OmniMake()<Cr>
" }}}

" OMNIRUN {{{
function! OmniRun()
    if !exists("g:OmniRun_cmd")
        let g:OmniRun_cmd = expand("%:p:h") . "/"
    endif
    call inputsave()
    let result = input("Run: ", g:OmniRun_cmd, "shellcmd")
    call inputrestore()
    if result == ""
        return
    endif
    let g:OmniRun_cmd = result
    execute "!" . g:OmniRun_cmd
endfunction
nnoremap <M-.> :call OmniRun()<Cr><Cr>
" }}}

" SHOWHELP {{{
function! ShowHelp(name)
    let name = input("Symbol: ", substitute(a:name, "\<Cr>", " ", "g"))
    if name == ""
        return
    end

    if exists("b:ShowHelp_func")
        exec "let matched = " . b:ShowHelp_func . "(name)"
    else
        let matched = 0
    end

    if !matched
        let query = substitute(Urlencode('q', name), '%', '\\%', "g")
        silent exec "!firefox 'http://www.google.com/search?".query."' &"
    end
    exec "normal \<C-l>"
endfunction

inoremap <F1> <C-o>:call ShowHelp("<C-r><C-w>")<Cr>
nnoremap <F1> :call ShowHelp("<C-r><C-w>")<Cr>
vnoremap <F1> y:call ShowHelp("<C-r>"")<Cr>
" }}}

" TABFIND {{{
" TabFind() {{{
" Local function, does the magic of reusing existing tabs.
function! <SID>TabFind(filename, ...)
    if  !filereadable(a:filename) | echoerr "File '".a:filename."' not found!" | endif

    let bufnr = bufnr(a:filename)

    " Don't create a new tab when on an empty buffer.
    if bufname("") == ""
        let edit_cmd = "edit "
    else
        let edit_cmd = "tabedit "
    endif

    " Is the file in the current tab? Avoid jumping to another tab containg
    " the same file.
    if bufwinnr(a:filename) != -1
        exec bufwinnr(bufnr)."wincmd w"
        if a:0 >= 1 | silent! exec a:1 | endif
        return
    endif

    " Open a new tab if no other tabs contain the file
    if  bufnr == -1
        exec edit_cmd.escape(a:filename, " ")
        exec bufwinnr(bufnr)."wincmd w"
        if a:0 >= 1 | silent! exec a:1 | endif
        return
    endif

    " Select the most recent tab with the file
    for tab in range(1, tabpagenr("$"))
        if index(tabpagebuflist(tab), bufnr) == -1 | continue | endif
        exec "tabnext ".tab
        exec bufwinnr(bufnr)."wincmd w"
        if a:0 >= 1 | silent! exec a:1 | endif
        return
    endfor

    " There must be hidden buffers then. We don't care :-)
    exec edit_cmd.escape(a:filename, " ")
    exec bufwinnr(bufnr)."wincmd w"
    if a:0 >= 1 | silent! exec a:1 | endif
    return
endfunction
" }}}

" Jump to tag while reusing open tabs and windows.
" TabTag() {{{
" Like <C-]>, but open a new tab or reuse another tab if it already contains
" the file. 
function! <SID>TabTag()
    let cword = expand("<cword>")
    if cword == "" | return | endif
    let taglist = taglist(cword)
    if len(taglist) == 0
        echoerr "Tag matching '".cword."' not found!"
        return
    endif
    let filename = ""
    " Select the first of the available tags.
    " TODO: Support choosing amongst the tags.
    for tag in taglist
        if tag["name"] == cword
            let filename = tag["filename"]
            break
        endif
    endfor
    if filename == ""
        print "Tag '".cword."' not found!"
        return
    endif

    call <SID>TabFind(filename, taglist[0]["cmd"])
endfunction
" }}}
nnoremap <M-}> :call <SID>TabTag()<Cr>

" Go to file while reusing open tabs and windows.
" TabFile() {{{
function! TabFile()
    let cfile = expand("<cfile>")

    " No need to do magic with absolute paths
    if cfile[0] == "/" 
        call <SID>TabFind(cfile) 
        return
    endif

    let sufadd = split(&suffixesadd, ",")
    " be sure to try with no extension first
    call insert(sufadd, "")
    " try with the original name first
    let files = [cfile]
    " and then with includeexpr applied
    if &includeexpr != ""
        execute "let inex_file = ".substitute(&includeexpr, "v:fname", "'".cfile."'", "")
        call add(files, inex_file)
    endif

    for exp_file in files
        let path = split(&path, ",")
        " expand wildcards etc
        let glob_path = []
        for dir in path
            " we can't use globpath() due to the prefixes
            let glob_path = glob_path + split(glob(dir),"\<NL>")
        endfor

        " ensure a default path
        if empty(glob_path) | call insert(glob_path, ".") | endif

        for dir in glob_path
            if dir == "" | continue | endif
            for suf in sufadd
                let file_suf = join([dir, exp_file], "/") . suf
                if filereadable(file_suf)
                    call <SID>TabFind(file_suf)
                    return
                endif
            endfor
        endfor
    endfor
    echo "File matching '".cfile."' not found!"
endfunction
" }}}

" Open file while reusing open tabs and windows.
" Opens file in a new tab, or reuse another tab if it already 
" contains the file. Supports &path (with wildcards), &includeexpr 
" and &suffixesadd.
" TabOpen() {{{
function! TabOpen(file)
    let cfile = a:file

    " No need to do magic with absolute paths
    if cfile[0] == "/" 
        call <SID>TabFind(cfile) 
        return
    endif

    let sufadd = split(&suffixesadd, ",")
    " be sure to try with no extension first
    call insert(sufadd, "")
    " try with the original name first
    let files = [cfile]
    " and then with includeexpr applied
    if &includeexpr != ""
        execute "let inex_file = ".substitute(&includeexpr, "v:fname", "'".cfile."'", "")
        call add(files, inex_file)
    endif

    for exp_file in files
        let path = split(&path, ",")
        " expand wildcards etc
        let glob_path = []
        for dir in path
            " we can't use globpath() due to the prefixes
            let glob_path = glob_path + split(glob(dir),"\<NL>")
        endfor

        " ensure a default path
        if empty(glob_path) | call insert(glob_path, ".") | endif

        for dir in glob_path
            if dir == "" | continue | endif
            for suf in sufadd
                let file_suf = join([dir, exp_file], "/") . suf
                if filereadable(file_suf)
                    call <SID>TabFind(file_suf)
                    return
                endif
            endfor
        endfor
    endfor
    echo "File matching '".cfile."' not found!"
endfunction
" }}}
command! -nargs=1 -complete=file TabOpen call TabOpen("<args>")
nnoremap <M-f> :TabOpen <C-d>
nnoremap <C-w><M-f> :call TabOpen(expand("<cfile>"))<Cr>
" }}}

" TOGGLING {{{

noremap <F5> :setl invhls<Cr>
noremap <F6> :setl invwrap<Cr>

" Cycle between languages.
" ToggleSpell() {{{
function! ToggleSpell()
    if &spell == 0
        setl spell spelllang=en
    elseif &spelllang == "en"
        setl spelllang=da
    else
        setl nospell spelllang=
    endif
endfunction
" }}}
noremap <F7> :call ToggleSpell()<Cr>
inoremap <F7> <C-o>:call ToggleSpell()<Cr>

set pastetoggle=<F8>

" }}}

" PLUGIN MAPPINGS {{{
nnoremap          <F8>  :windo call CloseIfTransientWindow()<Cr>
nnoremap          <F9>  :cwindow<Cr>
nmap     <silent> <F10> <Plug>ToggleProject
nnoremap          <F11> :TlistToggle<Cr>
nnoremap <silent> <F12> :!xterm &<Cr><Cr>
nnoremap <S-F11> :source ~/.vim/Session.vim<Cr>
nnoremap <S-F12> :mksession! ~/.vim/Session.vim<Cr>

imap <M-i> <Plug>IMAP_JumpForward
nmap <M-i> <Plug>IMAP_JumpForward
vmap <M-i> <Plug>IMAP_DeleteAndJumpForward

" Eclipse style commenting (<C-/>).
nnoremap <M-/> :call NERDComment(0, "invert")<CR>
xnoremap <M-/> :call NERDComment(1, "invert")<CR>gv
if !has('gui_running')
    nnoremap <C-_> :call NERDComment(0, "invert")<CR>
    xnoremap <C-_> :call NERDComment(1, "invert")<CR>gv
endif
" }}}

" SEARCH {{{
nnoremap - n
xnoremap - n
onoremap - n
nnoremap _ N
xnoremap _ N
onoremap _ N

" Emacs-like incremental search
cnoremap <expr> / (getcmdtype() == '/' \|\| getcmdtype() == '?' ? (getcmdline() == '' ? "\<Up>" : "\<Cr>/\<Up>") : "/")
cnoremap <expr> ? (getcmdtype() == '/' \|\| getcmdtype() == '?' ? (getcmdline() == '' ? "\<Up>" : "\<Cr>?\<Up>") : "?")

" next CamelCase word
onoremap m /\u\\|\s<Cr>
onoremap M ?\u\\|\s<Cr>

nnoremap <M-<> :cprevious<Cr>:cwindow<Cr>
nnoremap <M->> :cnext<Cr>:cwindow<Cr>
if !has("gui_running")
    " C-,
    nnoremap ù :cprevious<Cr>:cwindow<Cr>
    " C-.
    nnoremap û :cnext<Cr>:cwindow<Cr>
endif
" }}}

" INDENTATION {{{
xnoremap > >gv
xnoremap < <gv
nnoremap < <<
nnoremap > >>
" }}}

" CLIPBOARD {{{
nnoremap Y y$

xnoremap <M-c> "+y
nnoremap <M-c> "+yy
nnoremap <M-v> "+gp
cnoremap <M-v> <c-r><c-o>+
inoremap <M-v> <C-r><C-o>+
cnoremap <C-y> <C-r><C-o>"
inoremap <C-y> <C-r><C-o>"
" }}}

" COMPLETION {{{
" Vim tip #1386
inoremap <expr> <Cr>  pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
" Breaks escape sequences like home/end.
" inoremap <expr> <Esc> pumvisible() ? "\<C-e>" : "\<Esc>"
inoremap <expr> <C-n> pumvisible() ? '<C-n>'  : '<C-n><C-r>=pumvisible() ? "\<lt>Down>" : ""<CR>'
inoremap <expr> <C-p> pumvisible() ? '<C-p>'  : '<C-p><C-r>=pumvisible() ? "\<lt>Up>" : ""<CR>'
inoremap <expr> <C-@> pumvisible() ? '<C-n>'  : '<C-x><C-o><C-n><C-p><C-r>=pumvisible() ? "\<lt>Down>" : ""<CR>'
inoremap <expr> <C-]> pumvisible() ? '<C-n>'  : '<C-x><C-]><C-n><C-p><C-r>=pumvisible() ? "\<lt>Down>" : ""<CR>'
inoremap <expr> <M-,> pumvisible() ? '<C-n>'  : '<C-x><C-s><C-n><C-p><C-r>=pumvisible() ? "\<lt>Down>" : ""<CR>'
inoremap <expr> <M-.> pumvisible() ? '<C-n>'  : '<C-x><C-u><C-n><C-p><C-r>=pumvisible() ? "\<lt>Down>" : ""<CR>'

" 'Hippie completion' (closer to normal behavior).
inoremap <expr> <M-/> pumvisible() ? '<C-p>'  : '<C-p><C-r>=pumvisible() ? "\<lt>C-p>" : ""<CR>'
inoremap <expr> <M-?> pumvisible() ? '<C-n>'  : '<C-n><C-r>=pumvisible() ? "\<lt>C-n>" : ""<CR>'
" }}}


" CLEANUP {{{
smapclear
" }}}

" }}}1

" PLUGIN CONFIGURATION {{{1
" --------------------

" haskell-mode & haskell-indent {{{
let g:haddock_browser = "/usr/bin/firefox"
let g:haddock_indexfiledir = expand("$HOME/.local/var/cache/vim/")
let g:haddock_docdir = "/usr/share/doc/ghc6/"
" }}}

" imap {{{
let g:Imap_UsePlaceHolders     = 1

" Auxiliary function for imap mappings.
function! g:pad_cms()
    let pad_cms = substitute(&cms, '\S\zs \?%s', ' %s', "")
    let pad_cms = substitute(pad_cms, '%s \?\ze\S', '%s ', "")
    " fix TeX-comments
    let pad_cms = substitute(pad_cms, '% %s', '%% %s', "")
    return pad_cms
endfunction
" }}}

" LaTeX-suite {{{
set grepprg=grep\ -nH\ $*
let g:Tex_PromptedEnvironments = 'align*,array,lstlisting,verbatim,center,itemize,enumerate,description'
let g:Tex_HotKeyMappings       = 'align*'
let g:Tex_SmartKeyDot          = 0
let g:Tex_SmartKeyQuote        = 0

let g:Tex_CompileRule_dvi      = 'latex -src-specials -interaction=nonstopmode $*'
let g:Tex_UseEditorSettingInDVIViewer = 1
let g:Tex_ViewRule_pdf         = 'evince'
"let g:Tex_DefaultTargetFormat  = 'pdf'

let g:tex_flavor               = 'latex'
let g:tex_no_error=1
let g:tex_fold_enabled=1
let g:tex_indent_items=0
" }}}

" lisp {{{
let g:lisp_rainbow = 1
" }}}

" NERDCommenter {{{ 
"let g:NERDComInInsertMap='<C-c>'
let g:NERDShutUp = 1
let g:NERDSpaceDelims = 1
" }}}

" project {{{
let g:proj_flags="imst"
" }}}

" showmarks {{{
let g:showmarks_enable = 0
let g:showmarks_include="abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
let g:showmarks_ignore_type="hqmr"
let g:showmarks_textlower=" "
let g:showmarks_textupper=" "
let g:showmarks_textother=" "
" }}}

" taglist {{{
let Tlist_GainFocus_On_ToggleOpen = 1
let Tlist_Close_On_Select = 1
let Tlist_Inc_Winwidth = 0
let Tlist_Compact_Format = 1
let Tlist_Enable_Fold_Column = 0
let Tlist_Exit_OnlyWindow = 1
let Tlist_WinWidth = 22
let Tlist_Use_Right_Window = 1
" }}}

" timestamp {{{
let g:timestamp_regexp = '\v\C%(<%(Last %([cC]hanged?|modified)|Modified)\s*:\s+)\zs%(\d{4}-\d{2}-\d{2}|__TIMESTAMP)'
let g:timestamp_rep = '%Y-%m-%d'
" }}}

" vcscommand {{{
let g:VCSCommandMapPrefix = "<Leader>v"
" }}}

" netrw {{{
let g:netrw_alto            = 1
let g:netrw_altv            = 1
let g:netrw_browsex_viewer  = "exo-open"
let g:netrw_list_hide       = '^\.,\~$'
let g:netrw_liststyle       = 0
let g:netrw_preview         = 1
let g:netrw_special_syntax  = 1
let g:netrw_timefmt         = " %Y-%m-%d %H:%M"
let g:netrw_use_errorwindow = 0
let g:netrw_winsize	    = 24
" }}}

" xml {{{
let g:xml_syntax_folding = 1
" }}}
" }}}1

" vim: set ft=vim fdm=marker fdc=4 fdl=2:
