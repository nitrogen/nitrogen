

" if this has ft=nitrogen in the modeline
"     tell the system that it's actually an erlang filetype
"     but override the indentation rules such that it expands tabs to spaces,
"     uses tab width of 4 and uses vim's 'smartindent', which works much
"     better indentation rules for nitrogen elements.
if has("autocmd")
	autocmd FileType nitrogen set filetype=erlang smartindent expandtab shiftwidth=4 tabstop=4 indentexpr=""
endif
