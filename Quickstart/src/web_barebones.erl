-module (web_barebones).
-include ("wf.inc").
-compile(export_all).

main() -> #template { file="./templates/onecolumn.html" }.
title() -> "Bare Bones Page".
headline() -> "Bare Bones Page".

body() -> [
	"Nothing to see here."
].
	
event(_) -> ok.