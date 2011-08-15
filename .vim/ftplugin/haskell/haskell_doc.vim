"
" use haddock docs and index files
" show documentation, complete & qualify identifiers 
"
" (Claus Reinke; last modified: 18/06/2008)
" 
" part of haskell plugins: http://www.cs.kent.ac.uk/~cr3/toolbox/haskell/Vim/
" please send patches to <claus.reinke@talk21.com>

" :Doc <name> and :IDoc <name> open haddocks for <name> in opera
"
"   :Doc needs qualified name (default Prelude) and package (default base)
"   :IDoc needs unqualified name, looks up possible links in g:haddock_index
"
"   :DocIndex populates g:haddock_index from haddock's index files
"   :ExportDocIndex saves g:haddock_index to cache file
"   :ImportDocIndex reloads g:haddock_index from cache file
"
" all the following use the haddock index (g:haddock_index)
"
" _? opens haddocks for unqualified name under cursor, 
"    suggesting alternative full qualifications in popup menu
"
" _. fully qualifies unqualified name under cursor,
"    suggesting alternative full qualifications in popup menu
"
" _i  add import <module>(<name>) statement for unqualified <name> under cursor,
" _im add import <module>         statement for unqualified <name> under cursor,
"    suggesting alternative full qualifications in popup menu
"    (this currently adds one statement per call, instead of
"     merging into existing import statements, but it's a start;-)
"
" CTRL-X CTRL-U (user-defined insert mode completion) 
"   suggests completions of unqualified names in popup menu

let s:scriptname = "haskell_doc.vim"

" script parameters
"   g:haddock_browser            *mandatory* which browser to call
"   g:haddock_browser_callformat [optional] how to call browser
"   g:haddock_indexfiledir       [optional] where to put 'haddock_index.vim'
"   g:haddock_docdir             [optional] where to find html docs
"   g:ghc                        [optional] which ghc to call
"   g:ghc_pkg                    [optional] which ghc_pkg to call

" been here before?
if exists("g:haddock_index")
  finish
endif

" initialise nested dictionary, to be populated 
" - from haddock index files via :DocIndex
" - from previous cached version via :ImportDocIndex
let g:haddock_index = {}

" initialise dictionary, mapping modules with haddocks to their packages,
" populated via MkHaddockModuleIndex() or HaveModuleIndex()
let g:haddock_moduleindex = {}

" program to open urls, please set this in your vimrc
  "examples (for windows):
  "let g:haddock_browser = "C:/Program Files/Opera/Opera.exe"
  "let g:haddock_browser = "C:/Program Files/Mozilla Firefox/firefox.exe"
  "let g:haddock_browser = "C:/Program Files/Internet Explorer/IEXPLORE.exe"
if !exists("g:haddock_browser")
  echoerr s:scriptname." WARNING: please set g:haddock_browser!"
endif

if (!exists("g:ghc") || !executable(g:ghc)) 
  if !executable('ghc') 
    echoerr s:scriptname." can't find ghc. please set g:ghc, or extend $PATH"
    finish
  else
    let g:ghc = 'ghc'
  endif
endif    

if (!exists("g:ghc_pkg") || !executable(g:ghc_pkg))
  let g:ghc_pkg = substitute(g:ghc,'\(.*\)ghc','\1ghc-pkg','')
endif

if exists("g:haddock_docdir") && isdirectory(g:haddock_docdir)
  let s:docdir = g:haddock_docdir
elseif executable(g:ghc_pkg)
" try to figure out location of html docs
" first choice: where the base docs are
  let field = substitute(system(g:ghc_pkg . ' field base haddock-html'),'\n','','')
  let field = substitute(field,'haddock-html: \(.*\)libraries.base','\1','')
  let field = substitute(field,'\\','/','g')
  let alternate = substitute(field,'html','doc/html','')
  if isdirectory(field)
    let s:docdir = field
  elseif isdirectory(alternate)
    let s:docdir = alternate
  endif
else
  echoerr s:scriptname." can't find ghc-pkg (set g:ghc_pkg ?)."
endif

" second choice: try some known suspects for windows/unix
if !exists('s:docdir') || !isdirectory(s:docdir)
  let s:ghc_libdir = substitute(system(g:ghc . ' --print-libdir'),'\n','','')
  let location1a = s:ghc_libdir . '/doc/html/'
  let location1b = s:ghc_libdir . '/doc/'
  let s:ghc_version = substitute(system(g:ghc . ' --numeric-version'),'\n','','')
  let location2 = '/usr/share/doc/ghc-' . s:ghc_version . '/html/' 
  if isdirectory(location1a)
    let s:docdir = location1a
  elseif isdirectory(location1b)
    let s:docdir = location1b
  elseif isdirectory(location2)
    let s:docdir = location2
  else " give up
    echoerr s:scriptname." can't find locaton of html documentation (set g:haddock_docdir)."
    finish
  endif
endif

" todo: can we turn s:docdir into a list of paths, and
" include docs for third-party libs as well?

let s:libraries         = s:docdir . 'libraries/'
let s:guide             = s:docdir . 'users_guide/'
let s:index             = 'index.html'
if exists("g:haddock_indexfiledir") && filewritable(g:haddock_indexfiledir)
  let s:haddock_indexfiledir = g:haddock_indexfiledir 
elseif filewritable(s:libraries)
  let s:haddock_indexfiledir = s:libraries
elseif filewritable($HOME)
  let s:haddock_indexfiledir = $HOME.'/'
else "give up
  echoerr s:scriptname." can't locate index file. please set g:haddock_indexfiledir"
  finish
endif
let s:haddock_indexfile = s:haddock_indexfiledir . 'haddock_index.vim'

" different browser setups require different call formats;
" you might want to call the browser synchronously or 
" asynchronously, and the latter is os-dependent;
"
" by default, the browser is started in the background when on 
" windows or if running in a gui, and in the foreground otherwise
" (eg, console-mode for remote sessions, with text-mode browsers).
"
" you can override these defaults in your vimrc, via a format 
" string including 2 %s parameters (the first being the browser 
" to call, the second being the url).
if !exists("g:haddock_browser_callformat")
  if has("win32") || has("win64")
    let g:haddock_browser_callformat = 'start %s file://%s'
  else
    if has("gui_running")
      let g:haddock_browser_callformat = '%s file://%s '.printf(&shellredir,'/dev/null').' &'
    else
      let g:haddock_browser_callformat = '%s file://%s'
    endif
  endif
endif

" allow map leader override
if !exists("maplocalleader")
  let maplocalleader='_'
endif

command! DocSettings call DocSettings()
function! DocSettings()
  for v in ["g:haddock_browser","g:haddock_browser_callformat","g:haddock_docdir","g:haddock_indexfiledir","s:ghc_libdir","s:ghc_version","s:docdir","s:libraries","s:guide","s:haddock_indexfile"]
    if exists(v)
      echo v '=' eval(v)
    else
      echo v '='
    endif
  endfor
endfunction

function! DocBrowser(url)
  "echomsg "DocBrowser(".url.")"
  if (!exists("g:haddock_browser") || !executable(g:haddock_browser))
    echoerr s:scriptname." can't find documentation browser. please set g:haddock_browser"
    return
  endif
  " start browser to open url, according to specified format
  silent exe '!'.printf(g:haddock_browser_callformat,g:haddock_browser,escape(a:url,'#%')) 
endfunction

"usage examples:
" :Doc length
" :Doc Control.Monad.when
" :Doc Data.List.
" :Doc Control.Monad.State.runState mtl
" :Doc -top
" :Doc -libs
" :Doc -guide
command! -nargs=+ Doc  call Doc('v',<f-args>)
command! -nargs=+ Doct call Doc('t',<f-args>)

function! Doc(kind,qualname,...) 
  let suffix   = '.html'
  let relative = '#'.a:kind.'%3A'

  if a:qualname=="-top"
    call DocBrowser(s:docdir . s:index)
    return
  elseif a:qualname=="-libs"
    call DocBrowser(s:libraries . s:index)
    return
  elseif a:qualname=="-guide"
    call DocBrowser(s:guide . s:index)
    return
  endif

  if a:0==0 " no package specified
    let package = 'base/'
  else
    let package = a:1 . '/'
  endif

  if match(a:qualname,'\.')==-1 " unqualified name
    let [qual,name] = [['Prelude'],a:qualname]
    let file = join(qual,'-') . suffix . relative . name
  elseif a:qualname[-1:]=='.' " module qualifier only
    let parts = split(a:qualname,'\.')
    let quallen = len(parts)-1
    let [qual,name] = [parts[0:quallen],parts[-1]]
    let file = join(qual,'-') . suffix
  else " qualified name
    let parts = split(a:qualname,'\.')
    let quallen = len(parts)-2
    let [qual,name] = [parts[0:quallen],parts[-1]]
    let file = join(qual,'-') . suffix . relative . name
  endif

  let path = s:libraries . package . file
  call DocBrowser(path)
endfunction

" TODO: add commandline completion for :IDoc
" indexed variant of Doc, looking up links in g:haddock_index
" usage:
"  1. :IDoc length
"  2. click on one of the choices, or select by number (starting from 0)
command! -nargs=+ IDoc call IDoc(<f-args>)
function! IDoc(name,...) 
  let choices = HaddockIndexLookup(a:name)
  if choices=={} | return | endif
  if a:0==0
    let choice = inputlist(keys(choices))
  else
    let choice = a:1
  endif

  let path = s:libraries . values(choices)[choice]
  call DocBrowser(path)
endfunction

let s:flagref = s:guide . 'flag-reference.html'
if filereadable(s:flagref)
  " extract the generated fragment ids for the 
  " flag reference sections 
  let s:headerPat     = '.\{-}<h3 class="title"><a name="\([^"]*\)"><\/a>\([^<]*\)<\/h3>\(.*\)'
  let s:flagheaders   = []
  let s:flagheaderids = {}
  let s:contents      = join(readfile(s:flagref))
  let s:ml = matchlist(s:contents,s:headerPat)
  while s:ml!=[]
    let [_,s:id,s:title,s:r;s:x] = s:ml
    let s:flagheaders            = add(s:flagheaders, s:title)
    let s:flagheaderids[s:title] = s:id
    let s:ml = matchlist(s:r,s:headerPat)
  endwhile
  command! -nargs=1 -complete=customlist,CompleteFlagHeaders
          \ FlagReference call FlagReference(<f-args>)
  function! FlagReference(section)
    let relativeUrl = a:section==""||!exists("s:flagheaderids['".a:section."']") ? 
                    \ "" : "#".s:flagheaderids[a:section]
    call DocBrowser(s:flagref.relativeUrl)
  endfunction
  function! CompleteFlagHeaders(al,cl,cp)
    let s:choices = s:flagheaders
    return CompleteAux(a:al,a:cl,a:cp)
  endfunction
endif

command! -nargs=1 -complete=customlist,CompleteHaddockModules MDoc call MDoc(<f-args>)
function! MDoc(module)
  let suffix   = '.html'
  call HaveModuleIndex()
  if !has_key(g:haddock_moduleindex,a:module)
    echoerr a:module 'not found in haddock module index'
    return
  endif
  let package = g:haddock_moduleindex[a:module]
  let file    = substitute(a:module,'\.','-','g') . suffix
  let path    = s:libraries . package . '/' . file
  call DocBrowser(path)
endfunction

function! CompleteHaddockModules(al,cl,cp)
  call HaveModuleIndex()
  let s:choices = keys(g:haddock_moduleindex)
  return CompleteAux(a:al,a:cl,a:cp)
endfunction

" create a dictionary g:haddock_index, containing the haddoc index
command! DocIndex call DocIndex()
function! DocIndex()
  let files   = split(globpath(s:libraries,'doc-index-*.html'),'\n')
  "let files   = [s:libraries.'doc-index-33.html']
  let entryPat= '.\{-}"indexentry"[^>]*>\([^<]*\)<\(\%([^=]\{-}TD CLASS="\%(indexentry\)\@!.\{-}</TD\)*\)[^=]\{-}\(\%(="indexentry\|TABLE\).*\)'
  let linkPat = '.\{-}HREF="\([^"]*\)".>\([^<]*\)<\(.*\)'
  let g:haddock_index = {}

  redraw
  echo 'populating g:haddock_index from haddock index files in ' s:libraries
  for file in files  
    echo file[len(s:libraries):]
    let contents = join(readfile(file))
    let ml = matchlist(contents,entryPat)
    while ml!=[]
      let [_,entry,links,r;x] = ml
      "echo entry links
      let ml2 = matchlist(links,linkPat)
      let link = {}
      while ml2!=[]
        let [_,l,m,links;x] = ml2
        "echo l m
        let link[m] = l
        let ml2 = matchlist(links,linkPat)
      endwhile
      let g:haddock_index[DeHTML(entry)] = deepcopy(link)
      "echo entry g:haddock_index[entry]
      let ml = matchlist(r,entryPat)
    endwhile
  endfor
  return 1
endfunction

command! ExportDocIndex call ExportDocIndex()
function! ExportDocIndex()
  call HaveIndex()
  let entries = []
  for key in keys(g:haddock_index)
    let entries += [key,string(g:haddock_index[key])]
  endfor
  call writefile(entries,s:haddock_indexfile)
  redir end
endfunction

command! ImportDocIndex call ImportDocIndex()
function! ImportDocIndex()
  if filereadable(s:haddock_indexfile)
    let lines = readfile(s:haddock_indexfile)
    let i=0
    while i<len(lines)
      let [key,dict] = [lines[i],lines[i+1]]
      sandbox let g:haddock_index[key] = eval(dict) 
      let i+=2
    endwhile
    return 1
  else
    return 0
  endif
endfunction

function! HaveIndex()
  return (g:haddock_index!={} || ImportDocIndex() || DocIndex() )
endfunction

function! MkHaddockModuleIndex()
  let g:haddock_moduleindex = {}
  call HaveIndex()
  for key in keys(g:haddock_index)
    let dict = g:haddock_index[key]
    for module in keys(dict)
      let [_,package;x] = matchlist(dict[module],'^\([^\/]*\)\/')
      let g:haddock_moduleindex[module] = package
    endfor
  endfor
endfunction

function! HaveModuleIndex()
  return (g:haddock_moduleindex!={} || MkHaddockModuleIndex() )
endfunction

" decode HTML symbol encodings (are these all we need?)
function! DeHTML(entry)
  let res = a:entry
  let decode = { '&lt;': '<', '&gt;': '>', '&amp;': '\\&' }
  for enc in keys(decode)
    exe 'let res = substitute(res,"'.enc.'","'.decode[enc].'","g")'
  endfor
  return res
endfunction

" find haddocks for word under cursor
" also lists possible definition sites
noremap <LocalLeader>? :call Haddock()<cr>
function! Haddock()
  amenu ]Popup.- :echo '-'<cr>
  aunmenu ]Popup
  let namsym   = Haskell_GetNameSymbol(getline('.'),col('.'),0)
  if namsym==[]
    redraw
    echo 'no name/symbol under cursor!'
    return 0
  endif
  let [start,symb,qual,unqual] = namsym
  let imports = Haskell_GatherImports()
  let asm  = has_key(imports[1],qual) ? imports[1][qual]['modules'] : []
  let name = unqual
  let dict = HaddockIndexLookup(name)
  if dict=={} | return | endif
  let keys = ((qual=='')||(qual==asm[0])) ? keys(dict) : Haskell_ListIntersect(keys(dict),asm)
  if (qual!='') && (qual==asm[0])
    for key in keys
      if key==qual
        call DocBrowser(s:libraries . dict[key])
      endif
    endfor
  elseif has("gui_running")
    let i=0
    for key in keys
      exe 'amenu ]Popup.'.escape(key,'\.').' :call IDoc("'.escape(name,'|').'",'.i.')<cr>'
      let i+=1
    endfor
    popup ]Popup
  else
    let s:choices = keys
    let key = input('browse docs for '.name.' in: ','','customlist,CompleteAux')
    if key!=''
      call DocBrowser(s:libraries . dict[key])
    endif
  endif
endfunction

" used to pass on choices to CompleteAux
let s:choices=[]

" if there's no gui, use commandline completion instead of :popup
" completion function CompleteAux suggests completions for a:al, wrt to s:choices
function! CompleteAux(al,cl,cp)
  "echomsg '|'.a:al.'|'.a:cl.'|'.a:cp.'|'
  let res = []
  let l = len(a:al)-1
  for r in s:choices
    if l==-1 || r[0 : l]==a:al
      let res += [r]
    endif
  endfor
  return res
endfunction

" CamelCase shorthand matching: 
" favour upper-case letters and module qualifier separators (.) for disambiguation
function! CamelCase(shorthand,string)
  let s1 = a:shorthand
  let s2 = a:string
  let notFirst = 0 " don't elide before first pattern letter
  while ((s1!="")&&(s2!="")) 
    let head1 = s1[0]
    let head2 = s2[0]
    let elide = notFirst && ( ((head1=~'[A-Z]') && (head2!~'[A-Z.]')) 
              \             ||((head1=='.') && (head2!='.')) ) 
    if elide
      let s2=s2[1:]
    elseif (head1==head2) 
      let s1=s1[1:]
      let s2=s2[1:]
    else
      return 0
    endif
    let notFirst = (head1!='.')||(head2!='.') " treat separators as new beginnings
  endwhile
  return (s1=="")
endfunction

" use haddock name index for insert mode completion (CTRL-X CTRL-U)
function! CompleteHaddock(findstart, base)
  if a:findstart 
    let namsym   = Haskell_GetNameSymbol(getline('.'),col('.'),-1) " insert-mode: we're 1 beyond the text
    if namsym==[]
      redraw
      echo 'no name/symbol under cursor!'
      return -1
    endif
    let [start,symb,qual,unqual] = namsym
    return (start-1)
  else " find keys matching with "a:base"
    let res  = []
    let l    = len(a:base)-1
    let qual = a:base =~ '^[A-Z][a-zA-Z0-9_'']*\(\.[A-Z][a-zA-Z0-9_'']*\)*\(\.[a-zA-Z0-9_'']*\)\?$'
    call HaveIndex() 
    for key in keys(g:haddock_index)
      if (key[0 : l]==a:base)
        for m in keys(g:haddock_index[key])
          let res += [{"word":key,"menu":m,"dup":1}]
        endfor
      elseif qual " this tends to be slower
        for m in keys(g:haddock_index[key])
          let word = m . '.' . key
          if word[0 : l]==a:base
            let res += [{"word":word,"menu":m,"dup":1}]
          endif
        endfor
      endif
    endfor
    if res==[] " no prefix matches, try CamelCase shortcuts
      for key in keys(g:haddock_index)
        if CamelCase(a:base,key)
          for m in keys(g:haddock_index[key])
            let res += [{"word":key,"menu":m,"dup":1}]
          endfor
        elseif qual " this tends to be slower
          for m in keys(g:haddock_index[key])
            let word = m . '.' . key
            if CamelCase(a:base,word)
              let res += [{"word":word,"menu":m,"dup":1}]
            endif
          endfor
        endif
      endfor
    endif
    return res
  endif
endfunction
set completefunc=CompleteHaddock
set completeopt=menu,menuone,longest

" fully qualify an unqualified name
noremap <LocalLeader>. :call Qualify()<cr>
function! Qualify()
  amenu ]Popup.- :echo '-'<cr>
  aunmenu ]Popup
  let namsym   = Haskell_GetNameSymbol(getline('.'),col('.'),0)
  if namsym==[]
    redraw
    echo 'no name/symbol under cursor!'
    return 0
  endif
  let [start,symb,qual,unqual] = namsym
  if qual!=''  " TODO: should we support re-qualification?
    redraw
    echo 'already qualified'
    return 0
  endif
  let name = unqual
  let line         = line('.')
  let prefix       = (start<=1 ? '' : getline(line)[0:start-2] )
  let i=0
  let dict   = HaddockIndexLookup(name)
  if dict=={} | return | endif
  if has("gui_running")
    for key in keys(dict)
      let lhs=escape(prefix.name,'/.|\')
      let rhs=escape(prefix.key.'.'.name,'/&|\')
      exe 'amenu ]Popup.'.escape(key,'\.').' :'.line.'s/'.lhs.'/'.rhs.'/<cr>:noh<cr>'
      let i+=1
    endfor
    popup ]Popup
  else
    let s:choices = keys(dict)
    let key = input('qualify '.name.' with: ','','customlist,CompleteAux')
    if key!=''
      let lhs=escape(prefix.name,'/.\')
      let rhs=escape(prefix.key.'.'.name,'/&\')
      exe line.'s/'.lhs.'/'.rhs.'/'
      noh
    endif
  endif
endfunction

" create import for a (qualified) name
noremap <LocalLeader>i :call Import(0)<cr>
noremap <LocalLeader>im :call Import(1)<cr>
function! Import(module)
  amenu ]Popup.- :echo '-'<cr>
  aunmenu ]Popup
  let namsym   = Haskell_GetNameSymbol(getline('.'),col('.'),0)
  if namsym==[]
    redraw
    echo 'no name/symbol under cursor!'
    return 0
  endif
  let [start,symb,qual,unqual] = namsym
  let name       = unqual
  let pname      = ( symb ? '('.name.')' : name )
  let importlist = a:module ? '' : '('.pname.')'

  if qual!=''
    exe 'call append(search(''\%1c\(\<import\>\|\<module\>\|{-# OPTIONS\|{-# LANGUAGE\)'',''nb''),''import '.qual.importlist.''')'
    return
  endif

  let line   = line('.')
  let prefix = getline(line)[0:start-1]
  let dict   = HaddockIndexLookup(name)
  if dict=={} | return | endif
  if has("gui_running")
    for key in keys(dict)
      " exe 'amenu ]Popup.'.escape(key,'\.').' :call append(search("\\%1c\\(import\\\\|module\\\\|{-# OPTIONS\\)","nb"),"import '.key.importlist.'")<cr>'
      exe 'amenu ]Popup.'.escape(key,'\.').' :call append(search(''\%1c\(\<import\>\\|\<module\>\\|{-# OPTIONS\\|{-# LANGUAGE\)'',''nb''),''import '.key.escape(importlist,'|').''')<cr>'
    endfor
    popup ]Popup
  else
    let s:choices = keys(dict)
    let key = input('import '.name.' from: ','','customlist,CompleteAux')
    if key!=''
      exe 'call append(search(''\%1c\(\<import\>\|\<module\>\|{-# OPTIONS\|{-# LANGUAGE\)'',''nb''),''import '.key.importlist.''')'
    endif
  endif
endfunction

function! HaddockIndexLookup(name)
  call HaveIndex()
  if !has_key(g:haddock_index,a:name)
    echoerr a:name 'not found in haddock index'
    return {}
  endif
  return g:haddock_index[a:name]
endfunction
