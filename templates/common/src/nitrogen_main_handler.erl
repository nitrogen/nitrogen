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
    %% This ensures that this page will never be cached by the browser, even with the back/forward buttons
    %% You may want to override this on indivual pages either for CDN purposes, or just in general
    wf:header('cache-control', "no-cache, no-store, private, max-age=0"),
    wf_core:run().
