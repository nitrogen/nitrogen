% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (render_handler).
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
	
	% render_firstrequest(Context, State) -> {ok, NewContext, NewState, ResponseBridge}.
	% Render the elements and actions stored in Context.
	{render_firstrequest, 2},
	
	% render_postback(Context, State) -> {ok, NewContext, NewState, ResponseBridge}.
	% Render the elements and actions stored in Context.
	{render_postback, 2}
];

behaviour_info(_) -> undefined.
