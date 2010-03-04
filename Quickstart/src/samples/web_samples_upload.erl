-module (web_samples_upload).
-include_lib ("nitrogen/include/wf.hrl").
-compile(export_all).

main() -> #template { file="./templates/onecolumn.html", bindings=[
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

start_upload_event(myUpload1) ->
	wf:flash("Upload started.").
	
finish_upload_event(_Tag, undefined, _, _) ->
	wf:flash("Please select a file."),
	ok;

finish_upload_event(_Tag, FileName, LocalFileData, Node) ->
	FileSize = filelib:file_size(LocalFileData),
	wf:flash(wf:f("Uploaded file: ~s (~p bytes) on node ~s.", [FileName, FileSize, Node])),
	ok.
