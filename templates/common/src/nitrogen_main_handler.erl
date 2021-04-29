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
    %%   nitrogen:handler(MySecurityHandler, HandlerConfig),
    %%
    %%
    %% The following enables the debug_crash_handler for development. If you
    %% wish to use the default_crash_handler, which just spits out "Internal
    %% Server Error", comment or delete this next line.
    nitrogen:handler(debug_crash_handler, []),
    ok.

ws_init() ->
    handlers().

run() ->
    handlers(),
    wf_core:run().
