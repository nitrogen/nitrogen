% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_template).
-include ("wf.inc").
-compile(export_all).

% TODO - Revisit parsing in the to_module_callback. This
% will currently fail if we encounter a string like:
% "String with ) will fail" 
% or 
% "String with ]]] will fail"


reflect() -> record_info(fields, template).

render(_ControlID, Record) ->
	% Prevent loops.
	case wf:state(template_was_called) of
		true -> throw("Calling a template from a template.");
		_ -> ignore
	end,
	wf:state(template_was_called, true),
	
	% Parse the template file, or read it from cache.
	File = wf:to_list(Record#template.file),
	Template = parse_template(File),
	
	% IN PROGRESS - Caching
	% Key = {template, File},
	% Template = wf_cache:cache(Key, fun() -> parse_template(File) end, [{ttl, 5}]),
	
	% Evaluate the template.
	eval(Template, Record).


parse_template(File) ->
	case file:read_file(File) of
		{ok, B} -> parse_template1(B);
		_ -> 
			?LOG("Error reading file: ~s~n", [File]),
			wf:f("File not found: ~s.", [File])
	end.

parse_template1(B) ->
	F = fun(Tag) -> 
		try 
			Tag1 = wf:to_list(Tag),
			to_module_callback(Tag1) 
		catch _ : _ ->
			?LOG("Invalid template tag: ~s~n", [Tag])
		end
	end,
	parse(B, F).
	

%%% PARSE %%%
	
%% parse/2 - Given a binary and a callback, look through the binary
%% for strings of the form [[[module]]] or [[[module:function(args)]]]
parse(B, Callback) -> parse(B, Callback, []).
parse(<<>>, _Callback, Acc) -> [lists:reverse(Acc)];
parse(<<"[[[", Rest/binary>>, Callback, Acc) -> 
	{ Token, Rest1 } = get_token(Rest, <<>>),
	[lists:reverse(Acc), Callback(Token)|parse(Rest1, Callback, [])];
parse(<<C, Rest/binary>>, Callback, Acc) -> parse(Rest, Callback, [C|Acc]).
	
get_token(<<"]]]", Rest/binary>>, Acc) -> { Acc, Rest };
get_token(<<H, Rest/binary>>, Acc) -> get_token(Rest, <<Acc/binary, H>>).

to_module_callback("script") -> script;
to_module_callback(Tag) ->
	% Get the module...
	{ModuleString, Rest1} = peel(Tag, $:),
	Module = wf:to_atom(ModuleString),
	
	% Get the function...
	{FunctionString, Rest2} = peel(Rest1, $(),
	Function = wf:to_atom(FunctionString),
	
	{ArgString, Rest3} = peel(Rest2, $)),
	
	case Rest3 of
		[] -> [{Module, Function, ArgString}];
		_ ->  [{Module, Function, ArgString}|to_module_callback(tl(Rest3))]
	end.

peel(S, Delim) -> peel(S, Delim, []).
peel([], _Delim, Acc) -> {lists:reverse(Acc), []};
peel([Delim|T], Delim, Acc) -> {lists:reverse(Acc), T};
peel([H|T], Delim, Acc) -> peel(T, Delim, [H|Acc]).

to_term(X, Bindings) ->
	S = wf:to_list(X),
	{ok, Tokens, 1} = erl_scan:string(S),
	{ok, Exprs} = erl_parse:parse_exprs(Tokens),
	{value, Value, _} = erl_eval:exprs(Exprs, Bindings),
	Value.



%%% EVALUATE %%%

eval([], _) -> [];
eval([script|T], Record) -> [wf_script:get_script()|eval(T, Record)];
eval([H|T], Record) when ?IS_STRING(H) -> [H|eval(T, Record)];
eval([H|T], Record) -> [eval_callbacks(H, Record)|eval(T, Record)].
	
eval_callbacks([], _) -> [];
eval_callbacks([H|T], Record) ->
	{M, Function, ArgString} = H,
	
	% De-reference to page module...
	Module = case M of 
		page -> wf_platform:get_page_module();
		_ -> M
	end,

	% Convert args to term...
	Args = to_term("[" ++ ArgString ++ "].", Record#template.bindings),
	
	code:ensure_loaded(Module),
	case erlang:function_exported(Module, Function, length(Args)) of
		false -> 
			% Function is not defined, so try the next one.
			eval_callbacks(T, Record);
			
		true ->
			case erlang:apply(Module, Function, Args) of
				undefined -> 
					% Function returns undefined, so try the next one.
					eval_callbacks(T, Record);

				Data ->
					% Got some data. Render it if necessary.
					case wf:is_string(Data) of
						true -> Data;
						false -> wf:render(Data)
					end
			end
	end.