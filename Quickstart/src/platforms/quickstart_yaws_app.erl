-module(quickstart_yaws_app).
-export ([start/2, stop/0, out/1]).
-include ("wf.inc").
-define(PORT, 8000).

start(_, _) ->
  % Start Yaws...
  SL = [
    {port, 8000},
    {appmods, [{"/", ?MODULE}]}
  ],
  yaws:start_embedded("./wwwroot", SL),

  % Return the Yaws pid...
  Running = proplists:get_value(running, application:info()),
  Pid = proplists:get_value(yaws, Running),
  {ok, Pid}.

stop() ->
  application:stop(yaws).

out(Arg) ->
  RequestBridge = simple_bridge:make_request(yaws_request_bridge, Arg),
  ResponseBridge = simple_bridge:make_response(yaws_response_bridge, Arg),
  nitrogen:init_request(RequestBridge, ResponseBridge),
  nitrogen:run().
