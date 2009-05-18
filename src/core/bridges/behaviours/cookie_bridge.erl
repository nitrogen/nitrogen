% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (cache_bridge).
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
	
	% cookie(Key, Value, Path, MinutesToLive) -> ParameterizedModule
	% Set the specified cookie.
	{cookie, 4}
];

behaviour_info(_) -> undefined.
