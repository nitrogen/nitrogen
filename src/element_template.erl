% Nitrogen Web Framework for Erlang
% Copyright (c) 2008 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_template).
-include ("wf.inc").
-compile(export_all).

render(_ControlID, Record) ->
	FileName = Record#template.file,
	FilePath = io_lib:format("./content/web_content/~s.html", [FileName]),
	FileContents = case file:read_file(FilePath) of
		{ok, B} -> 
			B;
		_ -> 
			?LOG("Error reading file: ~s~n", [FilePath]),
			wf:f("File not %%%title%%% found: ~s.", [FilePath])
	end,

	replace(FileContents, fun(Tag) -> lookup(Tag, Record) end).
	
replace(<<>>, _Callback) -> [];
replace(<<"[[[", Rest/binary>>, Callback) -> 
	{ Token, Rest1 } = get_token(Rest, <<>>),
	Token1 = wf:to_atom(Token),
	[Callback(Token1)|replace(Rest1, Callback)];
replace(<<C, Rest/binary>>, Callback) -> [C, replace(Rest, Callback)].
	
get_token(<<"]]]", Rest/binary>>, Acc) -> { Acc, Rest };
get_token(<<H, Rest/binary>>, Acc) -> get_token(Rest, <<Acc/binary, H>>).
	
lookup(Tag, Record) ->
	case Tag of 
		title -> Record#template.title;
		headline -> Record#template.headline;
		section1 -> wf:render(Record#template.section1);
		section2 -> wf:render(Record#template.section1);
		section3 -> wf:render(Record#template.section1);
		section4 -> wf:render(Record#template.section1);
		script -> script;
		_ -> Tag:render_in_template(Record)
	end. 