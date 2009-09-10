% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

%
% The session_handler provides a place to store values on the server
% between requests.
%
% An application can define a custom session handler to control
% how Nitrogen manages session values.

-module (session_handler).
-export ([
	behaviour_info/1, 
	get_value/1, 
	get_value/2, 
	set_value/2, 
	clear_value/1, 
	clear_all/0
]).

% Example Session Handler Interface
behaviour_info(callbacks) -> [
	{init, 1},      
	{finish, 1},
	{get_value, 3},       
	{set_value, 3},
	{clear_value, 2},
	{clear_all, 1}
];
behaviour_info(_) -> undefined.

% get(Key, DefaultValue, State, Key, DefaultValue) -> {ok, Value, NewState}.
% Retrieve a value from the storage area.
get_value(Key) ->
	_Value = get_value(Key, undefined).
	
% get(Key, DefaultValue, State, Key, DefaultValue) -> {ok, Value, NewState}.
% Retrieve a value from the storage area.
get_value(Key, DefaultValue) ->
	_Value = wf_handler:call_readonly(session_handler, get_value, [Key, DefaultValue]).
	
% set_value(Key, Value, State) -> {ok, NewState}.
% Put a value into the storage area.
set_value(Key, Value) ->
	ok = wf_handler:call(session_handler, set_value, [Key, Value]).

% clear_value(Key, State) -> {ok, NewState}.
% Remove a value from the storage area.
clear_value(Key) ->
	ok = wf_handler:call(session_handler, clear_value, [Key]).

% clear_all(State) -> {ok, NewState}.
% Clear all values from the storage area.
clear_all() ->
	ok = wf_handler:call(session_handler, clear_all).
