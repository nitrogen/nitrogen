-module (web_nbe_4_3).
-include ("wf.inc").
-compile(export_all).

main() -> #template { file="./wwwroot/nbe.html" }.	

body() -> 
	[
		#label { text="Upload a file..." },
		#upload { tag=myupload, button_text="Upload File" },
		#p{},
		#span { id=myspan }
	].

upload_event(_Tag, undefined, _) ->
	wf:update(myspan, "Please select a file.");

upload_event(_Tag, FileName, LocalFileData) ->
	FileSize = filelib:file_size(LocalFileData),
	S = wf:f("Uploaded file: ~s (~p bytes)", [FileName, FileSize]),
	wf:update(myspan, S).