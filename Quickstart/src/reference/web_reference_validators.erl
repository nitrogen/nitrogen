-module (web_reference_validators).
-include ("wf.inc").
-compile(export_all).

main() -> #template { file="./wwwroot/onecolumn.html", bindings=[
	{'Group', reference},
	{'Item', validators}
]}.
	
title() -> "Nitrogen Validator Reference".
headline() -> "Nitrogen Validator Reference".

body() ->	[
	"Coming soon."
].

event(_) -> ok.
