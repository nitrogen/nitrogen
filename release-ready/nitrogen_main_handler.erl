-module(nitrogen_main_handler).
-export([
         run/0,
         ws_init/0
        ]).

handlers() ->
  nitrogen:handler([-$PROJECT-]_config_handler, []),
  nitrogen:handler(debug_crash_handler, []),
  ok.

ws_init() ->
  handlers().

run() ->
  handlers(),
  wf_core:run().
