-module (web_reference_api).
-include ("wf.inc").
-compile(export_all).

main() -> #template { file="./wwwroot/onecolumn.html", bindings=[
	{'Group', reference},
	{'Item', api}
]}.

title() -> "Nitrogen API Reference".
headline() -> "Nitrogen API Reference".

body() -> "Coming soon.".

event(_) -> ok.
