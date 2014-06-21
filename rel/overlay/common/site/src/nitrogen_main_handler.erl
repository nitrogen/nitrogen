-module(nitrogen_main_handler).
-export([run/0]).

run() ->
	%% Put any custom handlers here
	%% See http://nitrogenproject.com/doc/handlers.html
	%% Example:
	%%
	%%   wf_handler:set_handler(MySecurityHandler, HandlerConfig),
	%%
	wf_core:run().
