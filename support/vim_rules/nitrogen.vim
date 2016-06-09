

" if this has ft=nitrogen in the modeline
"     tell the system that it's actually an erlang filetype
"     but override the indentation rules such that it expands tabs to spaces,
"     uses tab width of 4 and uses vim's 'smartindent', which works much
"     better indentation rules for nitrogen elements.
if has("autocmd")
	" smartindenting will remove leading spaces when typing in a # as the
	" first character, so you don't want that with nitrogen, since *many,
	" many* lines will match that.  So the inoremap below fixes it
	autocmd Filetype nitrogen inoremap # X#
	autocmd FileType nitrogen set filetype=erlang smartindent autoindent expandtab shiftwidth=4 tabstop=4 softtabstop=4 indentexpr=""
endif
