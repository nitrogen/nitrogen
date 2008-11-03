-module (element_file).
-include ("wf.inc").
-compile(export_all).

render(ControlID, Record) -> 
	FileName = Record#file.file,
	FilePath = io_lib:format("./content/web_content/~s.html", [FileName]),
	FileContents = case file:read_file(FilePath) of
		{ok, B} -> 
			B;
		_ -> 
			?LOG("Error reading file: ~s~n", [FilePath]),
			wf:f("File not %%%title%%% found: ~s.", [FilePath])
	end,

	Panel = #panel {
		body=FileContents
	},
	
	element_panel:render(ControlID, Panel).
