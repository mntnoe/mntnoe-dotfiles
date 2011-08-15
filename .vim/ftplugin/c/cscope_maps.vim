""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" CSCOPE settings for vim           
"
" originally by Jason Duell       jduell@alumni.princeton.edu     2002/3/7
" modified by Mads N Noe
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" only one database import for now
if exists("g:did_cscope_maps")
    finish
endif

if has("cscope")

    """"""""""""" Standard cscope/vim boilerplate

    " use both cscope and ctag for 'ctrl-]', ':ta', and 'vim -t'
    set cscopetag

    " check cscope for definition of a symbol before checking ctags: set to 1
    " if you want the reverse search order.
    set csto=0

    " add any cscope database in current directory
    if filereadable("cscope.out")
        cs add cscope.out  
    " else add the database pointed to by environment variable 
    elseif $CSCOPE_DB != ""
        cs add $CSCOPE_DB
    endif

    " show msg when any other cscope db added
    set cscopeverbose  


    """"""""""""" My cscope/vim key mappings
    "
    " The following maps all invoke one of the following cscope search types:
    "
    "   's'   symbol: find all references to the token under cursor
    "   'g'   global: find global definition(s) of the token under cursor
    "   'c'   calls:  find all calls to the function name under cursor
    "   't'   text:   find all instances of the text under cursor
    "   'e'   egrep:  egrep search for the word under cursor
    "   'f'   file:   open the filename under cursor
    "   'i'   includes: find files that include the filename under cursor
    "   'd'   called: find functions that function under cursor calls
    "
    " All of the maps involving the <cfile> macro use '^<cfile>$': this is so
    " that searches over '#include <time.h>" return only references to
    " 'time.h', and not 'sys/time.h', etc. (by default cscope will return all
    " files that contain 'time.h' as part of their name).


    if has("!gui_running")
        noremap <buffer> <LocalLeader>ss :cs find s <C-R>=expand("<cword>")<CR><CR>	
        noremap <buffer> <LocalLeader>sg :cs find g <C-R>=expand("<cword>")<CR><CR>	
        noremap <buffer> <LocalLeader>sc :cs find c <C-R>=expand("<cword>")<CR><CR>	
        noremap <buffer> <LocalLeader>sx :cs find t <C-R>=expand("<cword>")<CR><CR>	
        noremap <buffer> <LocalLeader>se :cs find e <C-R>=expand("<cword>")<CR><CR>	
        noremap <buffer> <LocalLeader>sf :cs find f <C-R>=expand("<cfile>")<CR><CR>	
        noremap <buffer> <LocalLeader>si :cs find i ^<C-R>=expand("<cfile>")<CR>$<CR>
        noremap <buffer> <LocalLeader>sd :cs find d <C-R>=expand("<cword>")<CR><CR>	

        noremap <buffer> <LocalLeader>sts :tab scs find s <C-R>=expand("<cword>")<CR><CR>	
        noremap <buffer> <LocalLeader>stg :tab scs find g <C-R>=expand("<cword>")<CR><CR>	
        noremap <buffer> <LocalLeader>stc :tab scs find c <C-R>=expand("<cword>")<CR><CR>	
        noremap <buffer> <LocalLeader>stx :tab scs find t <C-R>=expand("<cword>")<CR><CR>	
        noremap <buffer> <LocalLeader>ste :tab scs find e <C-R>=expand("<cword>")<CR><CR>	
        noremap <buffer> <LocalLeader>stf :tab scs find f <C-R>=expand("<cfile>")<CR><CR>	
        noremap <buffer> <LocalLeader>sti :tab scs find i ^<C-R>=expand("<cfile>")<CR>$<CR>	
        noremap <buffer> <LocalLeader>std :tab scs find d <C-R>=expand("<cword>")<CR><CR>	
    else
        anoremenu CSCOPE.&Symbol<Tab>\\ss                      :cs find s <C-R>=expand("<cword>")<CR><CR>
        anoremenu CSCOPE.&Global\ Definition<Tab>\\sg          :cs find g <C-R>=expand("<cword>")<CR><CR>
        anoremenu CSCOPE.&Calls\ to\ Function<Tab>\\sc         :cs find c <C-R>=expand("<cword>")<CR><CR>
        anoremenu CSCOPE.Te&xt<Tab>\\sx                        :cs find t <C-R>=expand("<cword>")<CR><CR>
        anoremenu CSCOPE.&Egrep<Tab>\\se                       :cs find e <C-R>=expand("<cword>")<CR><CR>
        anoremenu CSCOPE.&Filename<Tab>\\sf                    :cs find f <C-R>=expand("<cfile>")<CR><CR>
        anoremenu CSCOPE.&Includes\ Filename<Tab>\\si          :cs find i ^<C-R>=expand("<cfile>")<CR>$<CR>
        anoremenu CSCOPE.Calle&d\ from\ Function<Tab>\\sd      :cs find d <C-R>=expand("<cword>")<CR><CR>

        anoremenu CSCOPE.&Tab\.\.\..&Symbol<Tab>\\sts                 :tab cs find s <C-R>=expand("<cword>")<CR><CR>
        anoremenu CSCOPE.&Tab\.\.\..&Global\ Definition<Tab>\\stg     :tab cs find g <C-R>=expand("<cword>")<CR><CR>
        anoremenu CSCOPE.&Tab\.\.\..&Calls\ to\ Function<Tab>\\stc    :tab cs find c <C-R>=expand("<cword>")<CR><CR>
        anoremenu CSCOPE.&Tab\.\.\..Te&xt<Tab>\\stx                   :tab cs find t <C-R>=expand("<cword>")<CR><CR>
        anoremenu CSCOPE.&Tab\.\.\..&Egrep<Tab>\\ste                  :tab cs find e <C-R>=expand("<cword>")<CR><CR>
        anoremenu CSCOPE.&Tab\.\.\..&Filename<Tab>\\stf               :tab cs find f <C-R>=expand("<cfile>")<CR><CR>
        anoremenu CSCOPE.&Tab\.\.\..&Includes\ Filename<Tab>\\sti     :tab cs find i ^<C-R>=expand("<cfile>")<CR>$<CR>
        anoremenu CSCOPE.&Tab\.\.\..Calle&d\ from\ Function<Tab>\\std :tab cs find d <C-R>=expand("<cword>")<CR><CR>

        noremap <buffer> <LocalLeader>s :popup CSCOPE<Cr>

    endif

let g:did_cscope_maps = 1

endif " has_cscope

" vim: set ft=vim:
