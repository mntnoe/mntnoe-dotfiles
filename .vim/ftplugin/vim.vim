setl keywordprg=

function! ShowHelp_vim(name)
    exec "help " . a:name
    return 1
endfunction
let b:ShowHelp_func = "ShowHelp_vim"
