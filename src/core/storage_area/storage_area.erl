% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (nitrogen_storage_area).
-export ([
	make/2, 
	behaviour_info/1
]).

behaviour_info(callbacks) -> [
	% init(Name, PageModule, RequestBridge) -> ParameterizedModule
	% Name is usually 'page', but could be something else.
	% Called first, before any other functions.
	{init, 2},      
	
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
	{clear_all, 0},
	
	% to_js() -> HTML
	% 
	{to_js, 0}    % Include JS on the page
];

behaviour_info(_) -> undefined.
