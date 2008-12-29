-module (web_samples_barebones).
-include ("wf.inc").
-compile(export_all).

main() -> #template { file="./wwwroot/onecolumn.html", bindings=[
	{'Group', learn},
	{'Item', samples}
]}.

title() -> "Bare Bones Page".
headline() -> "Bare Bones Page".
right() -> linecount:render().

body() -> [
	"Nothing to see here."
].
	
event(_) -> ok.