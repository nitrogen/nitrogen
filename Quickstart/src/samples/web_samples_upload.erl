-module (web_samples_upload).
-include ("wf.inc").
-compile(export_all).

main() -> #template { file="./wwwroot/onecolumn.html", bindings=[
	{'Group', learn},
	{'Item', samples}
]}.

title() -> "Upload Example".
headline() -> "Upload Example".
right() -> linecount:render().

body() -> [
	#upload { id=myUpload }
].
	
event(_) -> ok.