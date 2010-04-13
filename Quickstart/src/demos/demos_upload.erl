-module (demos_upload).
-include_lib ("nitrogen/include/wf.hrl").
-compile(export_all).

main() -> #template { file="./templates/demos46.html" }.

title() -> "File Upload".
headline() -> "File Upload".


left() -> 
    [
        "
        <p>
        The <code>#upload{}</code> element allows a user to upload a
        file into a scratch directory.

        <p>
        The element fires events when the upload starts and when the
        upload completes, and passes back original filename, the name
        of the file on disk, and the node to which the file was
        uploaded.
        ",
        linecount:render()
    ].

right() -> 
    [
        #flash {},
        #p{},
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
