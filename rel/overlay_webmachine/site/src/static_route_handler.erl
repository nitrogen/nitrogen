% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (static_route_handler).
-behaviour (route_handler).
-include_lib ("nitrogen_core/include/wf.hrl").
-export ([
    init/2, 
    finish/2
]).

%% @doc

%% The static route handler simply directs all requests to the
%% provided page module. This is used by the nitrogen_webmachine along
%% with the webmachine dispatch table to send requests through
%% webmachine to a Nitrogen page.

init(PageModule, State) ->
    wf_context:page_module(PageModule),
    {ok, State}.

finish(_Config, State) -> 
    {ok, State}.
