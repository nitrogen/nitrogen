-module (web_blog).
-include_lib ("nitrogen/include/wf.hrl").
-compile(export_all).

main() -> #template { file="./templates/onecolumn.html", bindings=[
	{'Group', discuss},
	{'Item', blog}
]}.

title() -> "Blog".
headline() -> "Nitrogen Blog".

body() -> "Coming soon.".
