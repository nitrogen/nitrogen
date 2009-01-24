% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.
%
%
% Sync is a collection of tools to easily deploy and start 
% Erlang applications to bare, remote Erlang nodes from your
% local node.
%
% 90% of people can get by using sync:deploy/2. This will
% copy all local files to a remote node, minus "./ebin", "./Mnesia*",
% and "./mnesia*". It will then start the specified applications
% on all remote nodes.
%
% If this does not provide the flexibility you need, then 
% try coding your own deployment script using a combination of
% mirror:dir/3 and sync:start/2. See sync:deploy/2 for starter
% code.
%
%
% QUICK DEPLOY:
% sync:deploy(Nodes, Applications)
% Deploy to remote nodes with reasonable defaults.
% - Nodes -> node or [node]
% - Apps  -> App or [App]
% - App   -> appname or {appname, ExtraArgs}
%
%
% START APPS ON REMOTE NODES:
% sync:start(Nodes, Apps)
% Start the specified apps on the specified nodes.
%
%
% COMPILE LOCALLY:
% sync:go()
% Provided for legacy purposes. Simply calls make:all().
%


-module(sync).
-export ([
	go/0,
	make/0, make/1,
	deploy/2,
	start/2,
	mkdir/2,
	add_path/2,
	
	% private
	remote/1
]).

-include ("wf.inc").

go() -> make(node()).

deploy(Nodes, Apps) -> 
	make(node()),
	mirror:dir(Nodes, ".", ["./ebin", "./Mnesia", "./mnesia", ".git", ".svn"]),
	mkdir(Nodes, "./ebin/"),
	make(Nodes),
	add_path(Nodes, "./ebin/"),
	start(Nodes, Apps),
	ok.	
	
start(Nodes, Apps) ->
	% Connect to nodes, make sure mirror file is out there.
	Nodes1 = normalize_nodes(Nodes),
	Apps1 = normalize_apps(Apps),
	[pong = net_adm:ping(X) || X <- Nodes1],
	c:nl(?MODULE),
	[ok=start_application(Nodes1, X) || X <- Apps1],
	ok.

%%% MAKE %%%

% make/0 - Run make:all([load]) across every node.
make() -> make([node()|nodes()]).
make(Nodes) ->
	Nodes1 = normalize_nodes(Nodes), 
	F = fun() -> up_to_date = make:all([load]) end,
	case rpc:multicall(Nodes1, ?MODULE, remote, [F]) of
		{_, []} -> ok;
		{_, BadNodes} -> throw(BadNodes)
	end,
	ok.

	
%%% START APPLICATION %%%

start_application(Nodes, App) ->
	% Function to load the application, set extra args, then start.
	F = fun() ->
		{AppName, ExtraArgs} = App,
		case application:load(AppName) of
			ok -> ok;
			{error, {already_loaded, _}} -> ok;
			Other1 -> 
				io:format("Error loading ~s on ~s: ~p~n", [AppName, node(), Other1]),
				throw(Other1)

		end,

		% Set properties.
		[ok=application:set_env(AppName, Key, Value) || {Key, Value} <- ExtraArgs],

		% Start the app.
		io:format("Starting application: ~s~n", [AppName]),
		case application:start(AppName) of
			ok -> ok;
			{error, {already_started, _}} -> ok;
			Other2 -> 
				io:format("Error starting ~s on ~s: ~p~n", [AppName, node(), Other2]),
				throw(Other2)
		end,		
		ok
	end,
	
	% Run the above function across specified nodes.
	case rpc:multicall(Nodes, ?MODULE, remote, [F]) of
		{_, []} -> ok;
		{_, BadNodes} -> throw(BadNodes)
	end,
	ok.

	
%%% PRIVATE FUNCTIONS %%%

normalize_nodes(Nodes) -> lists:flatten([Nodes]).
normalize_apps(Apps) -> 
	F = fun(App) ->
		case App of 
			{A, B} -> {A, B};
			A      -> {A, []}
		end
	end,
	Apps1 = lists:flatten([Apps]),
	[F(X) || X <- Apps1].

% remote/1 - Execute the supplied function remotely.
% Used as a helper for RPC calls.
remote(Function) -> Function().
	
mkdir(Nodes, Dir) ->
	Nodes1 = normalize_nodes(Nodes),
	F = fun() -> ok = filelib:ensure_dir(Dir ++ "/") end,
	case rpc:multicall(Nodes1, sync, remote, [F]) of
		{_, []} -> ok;
		{_, BadNodes} -> throw(BadNodes)
	end,
	ok.

add_path(Nodes, Dir) ->
	Nodes1 = normalize_nodes(Nodes),
	?PRINT(Nodes1),
	?PRINT(Dir),
	F = fun() -> ok = filelib:ensure_dir(Dir ++ "/"), true = code:add_path(Dir) end,
	case rpc:multicall(Nodes1, sync, remote, [F]) of
		{_, []} -> ok;
		{_, BadNodes} -> throw(BadNodes)
	end,
	ok.