" ******************************************************
function! StringsSepCat(separator, ...)
    let str = a:1
	let index = 2
    while index <= a:0
		exe "let cur=a:".index
		let str = str.a:separator.cur
        let index = index + 1
    endwhile
	return str
endfunction

" ******************************************************
function! CreateLatexParEndings()
	" Creates the regexp that searches for paragraph terminators
	return StringsSepCat('\|','^\t*\$\$', '^\t*\\item', '^\\begin', '^\\end', '^{\|^}', '^%')
endfunction

" ******************************************************
function! LatexParBegin()
	" Regular expression for begin/end of a paragraph
	let parEndings = CreateLatexParEndings()

	" Current position
	let here = line(".")
	
	" Paragraph begin
	exe ':normal }'
	exe ":normal {"
	let par_top = line(".")

	" First backward occurrence of a parEnding
	" Wrong index if we are at the last line, but does not matter
	exe ":".(here+1)
	silent exe "?".parEndings
	let str_top = line(".")

	"echo "para begins     at ".par_top
	"echo "prev occurrence at ".str_top

	if     str_top==here
		let pos = -1
	elseif str_top>here || str_top<par_top
		let pos = par_top
	else
		let pos = str_top+1
	endif

	" Moves to the par begin
	if (pos < 0)
		exe ":".here
	else
		exe ":".pos
	endif

	return pos

endfunction

" ******************************************************
function! LatexParEnd()
	let parEndings = CreateLatexParEndings()

	" Current position
	let here = line(".")
	
	" Paragraph end
	exe ':normal }'
	let par_bot = line(".")

	" First backward occurrence of a parEnding
	" Wrong index if we are at the first line, but does not matter
	exe ":".(here-1)
	silent exe "/".parEndings
	let str_bot = line(".")

	"echo "para ends       at ".par_bot
	"echo "next occurrence at ".str_bot

	if     str_bot==here
		let pos = -1
	elseif str_bot<here || str_bot>par_bot
		let pos = par_bot
	else
		let pos = str_bot-1
	endif

	" Moves to the par begin
	if (pos < 0)
		exe ":".here
	else
		exe ":".pos
	endif

	return pos

endfunction

" ******************************************************
function! FormatLatexPar()
	let here  = line(".")
	let top   = LatexParBegin()
	let bot   = LatexParEnd()

	if top==-1 || bot==-1
		"We are on top of a paragraph ending
		exe ":".(here+1)
	elseif bot<here || top>here
		"We are probably on an empty line
		exe ":".(here+1)
	else
		"We are in a standard paragraph

		" Formats the lines between top and bot
		silent exe ':'.top.','.bot.'!fmt --width=74'

		" Moves at the begin of the next paragraph
		exe ":".top
		exe ":".(LatexParEnd()+1)
	endif
endfunction

" Maps FormatPar function to Ctrl-J
nnoremap <buffer> <M-g>  :call FormatLatexPar()<Cr>
vnoremap <buffer> <M-g>  :!fmt --width=79<Cr>

