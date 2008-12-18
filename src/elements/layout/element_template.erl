% Nitrogen Web Framework for Erlang
% Copyright (c) 2008 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_template).
-include ("wf.inc").
-compile(export_all).

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
	Key = {template, File},
	Template = wf_cache:cache(Key, fun() -> parse_template(File) end, [{ttl, 5}]),
	
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
	{ModuleString, Rest} = peel(Tag, $:),
	Module = wf:to_atom(ModuleString),
	
	% Get the function...
	{FunctionString, ArgString1} = peel(Rest, $(),
	Function = wf:to_atom(FunctionString),
	
	% Get the arg string...
	ArgString2 = string:strip(ArgString1, both, $.),
	ArgString3 = string:strip(ArgString2, both, $(),
	ArgString4 = string:strip(ArgString3, both, $)),
	{Module, Function, ArgString4}.

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
eval([H|T], Record) when is_list(H) -> [H|eval(T, Record)];
eval([script|T], Record) -> [wf_script:get_script()|eval(T, Record)];
eval([H|T], Record) when is_tuple(H) ->
	{Module, Function, ArgString} = H,
	PageModule = wf_platform:get_page_module(),	
	Args = to_term("[" ++ ArgString ++ "].", Record#template.bindings),

	Data1 = case {Module, Function} of 
		{script, render_in_template}   -> script;
		{page, Function} -> erlang:apply(PageModule, Function, Args);
		{Module, Function} -> erlang:apply(Module, Function, Args)
	end,
	
		% Call render if it has not already been called.
	Data2 = case wf:is_string(Data1) orelse Data1 == script of
		true -> Data1;
		false -> wf:render(Data1)
	end,
	[Data2|eval(T, Record)].