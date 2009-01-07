% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_file).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, file).

render(ControlID, Record) -> 
	FileName = Record#file.file,
	FilePath = io_lib:format(FileName),
	FileContents = case file:read_file(FilePath) of
		{ok, B} -> 
			B;
		_ -> 
			?LOG("Error reading file: ~s~n", [FilePath]),
			wf:f("File not found: ~s.", [FilePath])
	end,

	Panel = #panel {
		body=FileContents
	},
	
	element_panel:render(ControlID, Panel).
