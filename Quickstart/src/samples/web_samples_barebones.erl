-module (web_samples_barebones).
-include_lib ("nitrogen/include/wf.hrl").
-compile(export_all).

main() -> #template { file="./templates/onecolumn.html", bindings=[
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
