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

render_element(_HtmlID, Record, Context) ->
	% % Prevent loops.
	% case wf:state(template_was_called) of
	% 	true -> throw("Calling a template from a template.");
	% 	_ -> ignore
	% end,
	% wf:state(template_was_called, true),
	
	% Parse the template file...
	File = wff:to_list(Record#template.file),
	Template = parse_template(File),
	
	% IN PROGRESS - Caching
	% Key = {template, File},
	% Template = wf_cache:cache(Key, fun() -> parse_template(File) end, [{ttl, 5}]),
	
	% Evaluate the template.
	Body = eval(Template, Record, Context),
	
	% TODO - Windex mode.
	% IsWindexMode = wf:q(windex) == ["true"],
	% case IsWindexMode of
	% 	true ->	[
	% 		wf:f("Nitrogen.$lookup('~s').$update(\"~s\");", [get(current_id), wf_utils:js_escape(Body)])
	% 	];
	% 	false -> Body
	% end.
	{ok, Body, Context}.


parse_template(File) ->
	% TODO - Templateroot
	% File1 = filename:join(nitrogen:get_templateroot(), File),
	File1 = File,
	case file:read_file(File1) of
		{ok, B} -> parse_template1(B);
		_ -> 
			?LOG("Error reading file: ~s~n", [File1]),
			wff:f("File not found: ~s.", [File1])
	end.

parse_template1(B) ->
	F = fun(Tag) -> 
		try 
			Tag1 = wff:to_list(Tag),
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
	Module = wff:to_atom(ModuleString),
	
	% Get the function...
	{FunctionString, Rest2} = peel(Rest1, $(),
	Function = wff:to_atom(FunctionString),
	
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
	S = wff:to_list(X),
	{ok, Tokens, 1} = erl_scan:string(S),
	{ok, Exprs} = erl_parse:parse_exprs(Tokens),
	{value, Value, _} = erl_eval:exprs(Exprs, Bindings),
	Value.



%%% EVALUATE %%%

eval([], _, _) -> [];
eval([script|T], Record, Context) -> [script|eval(T, Record, Context)];
eval([H|T], Record, Context) when ?IS_STRING(H) -> [H|eval(T, Record, Context)];
eval([H|T], Record, Context) -> [replace_callbacks(H, Record, Context)|eval(T, Record, Context)].

% Turn callbacks into a reference to #function {}.
replace_callbacks(CallbackTuples, Record, Context) ->
	Bindings = Record#template.bindings,
	Functions = [convert_callback_tuple_to_function(M, F, ArgString, Bindings, Context) || {M, F, ArgString} <- CallbackTuples],
	#function { function=Functions }.
	
% TODO - This is a hack. Not sure of the best way to do this.
% The idea is that we want to allow a user to either: 
% 
% 1) Specify a function that DOES take a Context parameter, signaling 
%    to Nitrogen that they will be coding in "Functional Mode" Nitrogen 
%    using wff.erl, or 
% 2) Specify a function that DOES NOT take a Context parameter, signaling
%    to Nitrogen that they will be coding in "Process Dictionary Mode",
%    using wf.erl which expects the context to be available via get(context).
%
% Let the hackery commence!
convert_callback_tuple_to_function(Module, Function, ArgString, Bindings, Context) ->
	% De-reference to page module...
	Module1 = case Module of 
		page -> wff:get_page_module(Context);
		_ -> Module
	end,
	
	% Wrap 	
	case string:str(ArgString, "Context") of
		1 -> wrap_with_context(Module1, Function, ArgString, Bindings);
		0 -> wrap_without_context(Module1, Function, ArgString, Bindings)
	end.

wrap_with_context(Module, Function, ArgString, Bindings) ->
	fun(Context) ->
		% Convert args to term...
		Bindings1 = lists:sort([{'Context', Context}|Bindings]),
		Args = to_term("[" ++ ArgString ++ "].", Bindings1),
		
		% If the function in exported, then call it. 
		% Otherwise return undefined...
		{module, Module} = code:ensure_loaded(Module),
		case erlang:function_exported(Module, Function, length(Args)) of
			true -> {ok, _Elements, _NewContext} = erlang:apply(Module, Function, Args);
			false -> {ok, undefined, Context}
		end
	end.

wrap_without_context(Module, Function, ArgString, Bindings) ->
	fun() ->
		% Convert args to term...
		Args = to_term("[" ++ ArgString ++ "].", Bindings),
		
		% If the function in exported, then call it. 
		% Otherwise return undefined...
		{module, Module} = code:ensure_loaded(Module),
		case erlang:function_exported(Module, Function, length(Args)) of
			true -> _Elements = erlang:apply(Module, Function, Args);
			false -> undefined
		end
	end.