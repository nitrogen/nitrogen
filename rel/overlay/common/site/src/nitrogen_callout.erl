-module(nitrogen_callout).
-behaviour(simple_bridge_callout).
-export([
		run/1
	]).

run(Bridge) ->
    nitrogen:init_request(Bridge),
	
	%% Put any custom handlers here

    nitrogen:run().
