" File:     xml.vim
" Author:   Mads N Noe <mntnoe (@) gmail.com>
" Modified: 2009-05-22
"
" XML filetype plugin. Funcions ParseTag and TagMatch et al by Devin Weaver
" see http://www.vim.org/scripts/script.php?script_id=301

if exists("b:my_did_ftplugin")
  finish
endif

setlocal iskeyword+=:,-

setlocal ts=2 sw=2 et
setlocal foldmethod=syntax
setlocal foldlevel=99

setlocal omnifunc=xmlcomplete#CompleteTags
setlocal omnifunc=htmlcomplete#CompleteTags

let g:xml_use_xhtml = 1
highlight Comment ctermfg=Blue

" This makes the '%' jump between the start and end of a single tag.
setlocal matchpairs+=<:>
setlocal commentstring=<!--%s-->

" Close tag (xmledit)
inoremap <buffer> <C-k> ><Esc>:call <SID>ParseTag()<Cr>
" Close tag (omnicompletion)
inoremap <buffer> <C-b> </<C-x><C-o>

" Remove outer tag
nnoremap <buffer> <LocalLeader>r vat<Esc>da>`<da>
" Goto parent tag (well sometimes; I'm working on it :-)
nnoremap <buffer> <LocalLeader>u vatat<Esc>`<

" Check well-formed
nnoremap <buffer> <LocalLeader>xx :%w !xmllint --noout -<Cr>
" Validate DTD
nnoremap <buffer> <LocalLeader>xd :%w !xmllint --noout --valid -<Cr>
" Format document
nnoremap <buffer> <LocalLeader>xf :%!xmllint --format -<Cr>
" Validate XML Schema (put URI in the "s register)
nnoremap <buffer> <LocalLeader>xs :call <SID>ValidateXMLSchema()<Cr>

if !exists("*s:ValidateXMLSchema")
function s:ValidateXMLSchema()
        execute "%w !xmllint --noout --schema `resolveuri " . @s . "` -"
endfunction
endif

"""""""""""""""
" From xmledit:

" Callback -> Checks for tag callbacks and executes them.
if !exists("*s:Callback")
function s:Callback( xml_tag, isHtml )
    let text = 0
    if a:isHtml == 1 && exists ("*HtmlAttribCallback")
        let text = HtmlAttribCallback (a:xml_tag)
    elseif exists ("*XmlAttribCallback")
        let text = XmlAttribCallback (a:xml_tag)
    endif       
    if text != '0'
        execute "normal! i " . text ."\<Esc>l"
    endif
endfunction
endif

" IsParsableTag -> Check to see if the tag is a real tag.
if !exists("*s:IsParsableTag")
function s:IsParsableTag( tag )
    " The "Should I parse?" flag.
    let parse = 1

    " make sure a:tag has a proper tag in it and is not a instruction or end tag.
    if a:tag !~ '^<[[:alnum:]_:\-].*>$'
        let parse = 0
    endif

    " make sure this tag isn't already closed.
    if strpart (a:tag, strlen (a:tag) - 2, 1) == '/'
        let parse = 0
    endif
    
    return parse
endfunction
endif

" ParseTag -> The major work hourse for tag completion.
if !exists("*s:ParseTag")
function s:ParseTag( )
    " Save registers
    let old_reg_save = @"
    let old_save_x   = @x

    if (!exists("g:xml_no_auto_nesting") && strpart (getline ("."), col (".") - 2, 2) == '>>')
        let multi_line = 1
        execute "normal! \"xX"
    else
        let multi_line = 0
    endif

    let @" = ""
    execute "normal! \"xy%%"
    let ltag = @"
    if (&filetype == 'html' || &filetype == 'xhtml') && (!exists ("g:xml_no_html"))
        let html_mode = 1
        let ltag = substitute (ltag, '[^[:graph:]]\+', ' ', 'g')
        let ltag = substitute (ltag, '<\s*\([^[:alnum:]_:\-[:blank:]]\=\)\s*\([[:alnum:]_:\-]\+\)\>', '<\1\2', '')
    else
        let html_mode = 0
    endif

    if <SID>IsParsableTag (ltag)
        " find the break between tag name and atributes (or closing of tag)
        let index = matchend (ltag, '[[:alnum:]_:\-]\+')

        let tag_name = strpart (ltag, 1, index - 1)
        if strpart (ltag, index) =~ '[^/>[:blank:]]'
            let has_attrib = 1
        else
            let has_attrib = 0
        endif

        " That's (index - 1) + 2, 2 for the '</' and 1 for the extra character the
        " while includes (the '>' is ignored because <Esc> puts the curser on top
        " of the '>'
        let index = index + 2

        " print out the end tag and place the cursor back were it left off
        if html_mode && tag_name =~? '^\(img\|input\|param\|frame\|br\|hr\|meta\|link\|base\|area\)$'
            if has_attrib == 0
                call <SID>Callback (tag_name, html_mode)
            endif
            if exists ("g:xml_use_xhtml")
                execute "normal! i /\<Esc>l"
            endif
        else
            if multi_line
                " Can't use \<Tab> because that indents 'tabstop' not 'shiftwidth'
                " Also >> doesn't shift on an empty line hence the temporary char 'x'
                "let com_save = &comments
                "set comments-=n:>
                execute "normal! a\<Cr>\<Cr>\<Esc>kAx\<Esc>>>$\"xx"
                "execute "set comments=" . com_save

                " restore registers
                let @" = old_reg_save
                let @x = old_save_x

                startinsert!
                return ""
            else
                if has_attrib == 0
                    call <SID>Callback (tag_name, html_mode)
                endif
                execute "normal! a</" . tag_name . ">\<Esc>" . index . "h"
            endif
        endif
    endif

    " restore registers
    let @" = old_reg_save
    let @x = old_save_x

    if col (".") < strlen (getline ("."))
        execute "normal! l"
        startinsert
    else
        startinsert!
    endif
endfunction
endif

" BuildTagName -> Grabs the tag's name for tag matching.
if !exists("*s:BuildTagName")
function s:BuildTagName( )
  "First check to see if we Are allready on the end of the tag. The / search
  "forwards command will jump to the next tag otherwise

  " Store contents of register x in a variable
  let b:xreg = @x 

  exec "normal! v\"xy"
  if @x=='>'
     " Don't do anything
  else
     exec "normal! />/\<Cr>"
  endif

  " Now we head back to the < to reach the beginning.
  exec "normal! ?<?\<Cr>"

  " Capture the tag (a > will be catured by the /$/ match)
  exec "normal! v/\\s\\|$/\<Cr>\"xy"

  " We need to strip off any junk at the end.
  let @x=strpart(@x, 0, match(@x, "[[:blank:]>\<C-J>]"))

  "remove <, >
  let @x=substitute(@x,'^<\|>$','','')

  " remove spaces.
  let @x=substitute(@x,'/\s*','/', '')
  let @x=substitute(@x,'^\s*','', '')

  " Swap @x and b:xreg
  let temp = @x
  let @x = b:xreg
  let b:xreg = temp
endfunction
endif

" TagMatch1 -> First step in tag matching.
" Brad Phelan: First step in tag matching.
if !exists("*s:TagMatch1")
function s:TagMatch1()
  " Save registers
  let old_reg_save = @"

  "Drop a marker here just in case we have a mismatched tag and
  "wish to return (:mark looses column position)
  normal! mz

  call <SID>BuildTagName()

  "Check to see if it is an end tag. If it is place a 1 in endtag
  if match(b:xreg, '^/')==-1
    let endtag = 0
  else
    let endtag = 1  
  endif

 " Extract the tag from the whole tag block
 " eg if the block =
 "   tag attrib1=blah attrib2=blah
 " we will end up with 
 "   tag
 " with no trailing or leading spaces
 let b:xreg=substitute(b:xreg,'^/','','g')

 " Make sure the tag is valid.
 " Malformed tags could be <?xml ?>, <![CDATA[]]>, etc.
 if match(b:xreg,'^[[:alnum:]_:\-]') != -1
     " Pass the tag to the matching 
     " routine
     call <SID>TagMatch2(b:xreg, endtag)
 endif
 " Restore registers
 let @" = old_reg_save
endfunction
endif


" TagMatch2 -> Second step in tag matching.
" Brad Phelan: Second step in tag matching.
if !exists("*s:TagMatch2")
function s:TagMatch2(tag,endtag)
  let match_type=''

  " Build the pattern for searching for XML tags based
  " on the 'tag' type passed into the function.
  " Note we search forwards for end tags and
  " backwards for start tags
  if a:endtag==0
     "let nextMatch='normal /\(<\s*' . a:tag . '\(\s\+.\{-}\)*>\)\|\(<\/' . a:tag . '\s*>\)'
     let match_type = '/'
  else
     "let nextMatch='normal ?\(<\s*' . a:tag . '\(\s\+.\{-}\)*>\)\|\(<\/' . a:tag . '\s*>\)'
     let match_type = '?'
  endif

  if a:endtag==0
     let stk = 1 
  else
     let stk = 1
  end

 " wrapscan must be turned on. We'll recored the value and reset it afterward.
 " We have it on because if we don't we'll get a nasty error if the search hits
 " BOF or EOF.
 let wrapval = &wrapscan
 let &wrapscan = 1

  "Get the current location of the cursor so we can 
  "detect if we wrap on ourselves
  let lpos = line(".")
  let cpos = col(".")

  if a:endtag==0
      " If we are trying to find a start tag
      " then decrement when we find a start tag
      let iter = 1
  else
      " If we are trying to find an end tag
      " then increment when we find a start tag
      let iter = -1
  endif

  "Loop until stk == 0. 
  while 1 
     " exec search.
     " Make sure to avoid />$/ as well as /\s$/ and /$/.
     exec "normal! " . match_type . '<\s*\/*\s*' . a:tag . '\([[:blank:]>]\|$\)' . "\<Cr>"

     " Check to see if our match makes sence.
     if a:endtag == 0
         if line(".") < lpos
             call <SID>MisMatchedTag (0, a:tag)
             break
         elseif line(".") == lpos && col(".") <= cpos
             call <SID>MisMatchedTag (1, a:tag)
             break
         endif
     else
         if line(".") > lpos
             call <SID>MisMatchedTag (2, '/'.a:tag)
             break
         elseif line(".") == lpos && col(".") >= cpos
             call <SID>MisMatchedTag (3, '/'.a:tag)
             break
         endif
     endif

     call <SID>BuildTagName()

     if match(b:xreg,'^/')==-1
        " Found start tag
        let stk = stk + iter 
     else
        " Found end tag
        let stk = stk - iter
     endif

     if stk == 0
        break
     endif    
  endwhile

  let &wrapscan = wrapval
endfunction
endif

" MisMatchedTag -> What to do if a tag is mismatched.
if !exists("*s:MisMatchedTag")
function s:MisMatchedTag( id, tag )
    "Jump back to our formor spot
    normal! `z
    normal zz
    echohl WarningMsg
    " For debugging
    "echo "Mismatched tag " . a:id . ": <" . a:tag . ">"
    " For release
    echo "Mismatched tag <" . a:tag . ">"
    echohl None
endfunction
endif

let b:my_did_ftplugin = 1

