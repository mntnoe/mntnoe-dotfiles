" File:     urlencode.vim
" Author:   Mads Navntoft Noe <mail@madsnoe.dk>
" Modified: 2011-02-01

if exists("loaded_urlencode")
    finish
endif
if (v:progname == "ex")
   finish
endif
let loaded_urlencode = 1

" See 'script-here' in the vim manual pages.
if !has('python')
    function! Urlencode(var, value)
        return var . "=" . value
    endfunction
else
    function! Urlencode(var, value)
        python << EOF
import urllib
from vim import eval, command

var = eval("a:var")
value = eval("a:value")

command("let result = '%s'" % urllib.urlencode({var: value}))
EOF
        return result
    endfunction
endif

