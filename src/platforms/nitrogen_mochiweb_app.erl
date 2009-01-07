% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (nitrogen_mochiweb_app).
-export([start/0, stop/0]).
-export([loop/2]).

start() ->
	% Initialize Nitrogen.
	wf:init(),

	% Start the Mochiweb server.
	Port = nitrogen:get_port(),
	DocumentRoot = nitrogen:get_wwwroot(),
	Options = [{ip, "0.0.0.0"}, {port, Port}],
	Loop = fun (Req) -> ?MODULE:loop(Req, DocumentRoot) end,
	mochiweb_http:start([{name, get_name()}, {loop, Loop} | Options]).
	
loop(Req, DocRoot) ->
	"/" ++ Path = Req:get(path),
	case Req:get(method) of
		Method when Method =:= 'GET'; Method =:= 'HEAD' ->
			case Path of
				"" -> wf_mochiweb:loop(Req, web_index);
				"web/" ++ _ -> wf_mochiweb:loop(Req);
				_ -> Req:serve_file(Path, DocRoot)
			end;
			
		'POST' ->
			case Path of
				"" -> wf_mochiweb:loop(Req, web_index);
				"web/" ++ _ -> wf_mochiweb:loop(Req);
				_ -> Req:not_found()
			end;
		_ ->
		Req:respond({501, [], []})
	end.
	
stop() -> 
	% Stop the mochiweb server.
	mochiweb_http:stop(get_name()),
	ok.
	
get_name() ->
	case application:get_application() of
		{ok, App} -> App;
		undefined -> nitrogen
	end.