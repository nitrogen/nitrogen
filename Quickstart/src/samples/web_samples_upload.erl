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
	
	#h3 { text="Upload Example #1" },
	#p{},
	"Upload example with modified button text. The default text is 'Upload'.",
	#p{},
	#upload { tag=myUpload1, button_text="Upload File" },
	
	#hr{},
	
	#h3 { text="Upload Example #2" },
	#p{},
	"This example hides he upload button. When the user selects a file it will automatically start uploading.",
	#p{},
	#upload { tag=myUpload1, show_button=false }
	
].
	
event(_) -> ok.

upload_event(_Tag, undefined, _) ->
	wf:flash("Please select a file."),
	ok;

upload_event(_Tag, FileName, LocalFileData) ->
	FileSize = filelib:file_size(LocalFileData),
	wf:flash(wf:f("Uploaded file: ~s (~p bytes)", [FileName, FileSize])),
	ok.