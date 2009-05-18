% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (keyvalue_bridge).
-export ([
	behaviour_info/1
]).

behaviour_info(callbacks) -> [
	% init(Context) -> ParameterizedModule
	% Called at the start of the request.
	{init, 1},      

	% render(Context) -> NewContext
	% Called at the end of the request, before sending
	% a response back to the browser.
	{render, 1},
	
	% get(Key, DefaultValue) -> Value
	% Retrieve a value from the storage area.
	{get, 2},       
	
	% put(Key, Value) -> ParameterizedModule
	% Put a value into the storage area.
	{put, 2},
	
	% clear(Key) -> ParameterizedModule
	% Remove a value from the storage area.
	{clear, 1},
	
	% clear_all() -> ParameterizedModule
	% Clear all values from the storage area.
	{clear_all, 0}
];

behaviour_info(_) -> undefined.
