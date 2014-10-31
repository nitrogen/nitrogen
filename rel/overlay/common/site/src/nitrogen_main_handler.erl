%% vim: ts=4 sw=4 et
-module(nitrogen_main_handler).
-export([
    run/0,
    ws_init/0
]).

handlers() ->
    %% Put any custom handlers here
    %% See http://nitrogenproject.com/doc/handlers.html
    %% Example:
    %%
    %%   wf_handler:set_handler(MySecurityHandler, HandlerConfig),
    %%
    ok.

ws_init() ->
    handlers().

run() ->
    handlers(),
    wf_core:run().
