% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (route_handler).
-export ([
	behaviour_info/1
]).

behaviour_info(callbacks) -> [
	% init(Context) -> {ok, NewContext, State}.
	% Called at the start of the request.
	{init, 1},      

	% finish(Context, State) -> {ok, NewContext, NewState}.
	% Called at the end of the request, before sending
	% a response back to the browser.
	{finish, 2},
	
	% route(Context, State) -> {ok, NewContext, NewState}.
	% Route the request by setting the page_module and path_info 
	% attributes of Context#context.
	{route, 2}
];

behaviour_info(_) -> undefined.
