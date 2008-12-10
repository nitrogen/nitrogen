% Nitrogen Web Framework for Erlang
% Copyright (c) 2008 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (mochiweb_helper).
-export([start/0, loop/2]).

start() ->
	% Initialize Nitrogen.
	wf:init(),
	
	% Start crypto...
	crypto:start(),

	% Start mochiweb server...
	DocRoot = "./content/wwwroot",
	Options = [
		{ip, "0.0.0.0"},
		{port, 8000}
	],

	Loop = fun (Req) -> ?MODULE:loop(Req, DocRoot) end,
	
	F = fun() -> 
		mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options]),
		receive after infinity -> ok end
	end,
	spawn(F),
	
	io:format("~n~n---~n"),
	io:format("Nitrogen is now running, using mochiweb_http:start().~n"),
	io:format("Open your browser to: http://localhost:8000~n"),
	io:format("---~n~n").

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
