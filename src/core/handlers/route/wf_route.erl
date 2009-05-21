% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (wf_route).
-export ([route/1]).

route(Context) ->
	wf_context:apply(Context, route, route).
