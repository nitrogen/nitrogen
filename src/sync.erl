-module(sync).
-compile(export_all).

% Sync helps you deploy Erlang applications to bare Erlang nodes. 
% It is like a lite version of OTP releases, with some extra stuff thrown in.
%
%
% To use:
%  - Create an Emakefile to build your application.
%  - Create a Mirrorfile.platform (see format below) to define how files should be mirrored.
%  - Create a Startfile.platform (see format below) to define how applications should be started.
%  - Run sync:go(Environment)
%  - This will:
%    - Re-compiles all modules locally with Emakefile.
%    - Connects to all nodes
%    - Syncs files specified by mirror to all nodes. (Ignoring the ignore list.)
%    - Compiles code on all nodes with Emakefile.
%    - Starts any applications that should be started.
%    - Stops any other applications.

% EXAMPLE Mirrorfile:
%
% [
%		{nodes, [sync1@127.0.0.1, sync2@127.0.0.1]},
%		
% 	{mirror, [
%			"./src",
% 		"./priv", 
% 		"./include", 
% 		"./wwwroot"
% 	]},
%
%		{ignore, [
%     "./ebin"
%		]}
% ].
%
%
%	EXAMPLE Startfile:
%
%	[
%		{Nodes, application, Args}
%		{Nodes, application, Args}
%		{Nodes, application, Args}
% ].
%
% Nodes = node or [node] or all
% ApplicationDef = one or many of Application or {Application, Args}

go() -> go(get_environment()).

go(Environment) -> 
	% Read configuration.
	{Nodes, Mirror, Ignore} = read_mirrorfile(Environment),
	{Applications} = read_startfile(Environment),
	
	% Re-compile files locally.
	make([node()]),
	
	% Connect to all nodes.
	connect_to_nodes(Nodes),
	
	% Send the sync module to all nodes.
	c:nl(?MODULE), 
	
	% Mirror files across all nodes.
	mirror(Nodes, Mirror, Ignore),
	
	% Compile files across all nodes.
	make(Nodes),
	
	% Start up applications.
	start(Applications),
	ok.



%%% READ CONFIGURATION %%%

read_mirrorfile(Environment) ->
	S = atom_to_list(Environment),
	case file:consult("Mirrorfile." ++ S) of
		{ok, [Term]} -> 
			{
				proplists:get_value(nodes, Term), 
				proplists:get_value(mirror, Term), 
				proplists:get_value(ignore, Term)
			};
			
		{ok, _Terms} -> throw("Only one term allowed in Mirrorfile." ++ S ++ "!");
		{error, enoent} -> {[],[],[]};
		{error, Reason} -> throw(Reason)
	end.
	
read_startfile(Environment) ->
	S = atom_to_list(Environment),
	case file:consult("Startfile." ++ S) of
		{ok, [Term]} -> {Term};
		{ok, _Terms} -> throw("Only one term allowed in Startfile." ++ S ++ "!");
		{error, enoent} -> {[]};
		{error, Reason} -> throw(Reason)
	end.

	% Nodes = ['sync2@127.0.0.1', 'sync3@127.0.0.1'],
	% Mirror = ["./*"],
	% Ignore = ["./ebin", "./start.sh"],
	% Apps = [
	% 	{['sync2@127.0.0.1'], mnesia, []},
	% 	{['sync2@127.0.0.1'], crypto, []}
	% ],



%%% CONNECT TO NODES %%%	

% connect_to_nodes/1 - Given a list of nodes, ping each one and look for the pong.
connect_to_nodes([]) -> 
	case nodes() of
		[] -> ignore;
		_  -> io:format("Connected to nodes: ~p~n", [nodes()])
	end,
	ok;
	
connect_to_nodes([H|T]) -> 
	pong = net_adm:ping(H),
	connect_to_nodes(T).



%%% MAKE %%%

% make/0 - Run make:all across every node.
make() -> make([node()|nodes()]).
make(Nodes) -> 
	F = fun() -> up_to_date = make:all([load]) end,
	case rpc:multicall(Nodes, ?MODULE, remote, [F]) of
		{_, []} -> ok;
		{_, BadNodes} -> throw(BadNodes)
	end,
	ok.

%%% COMPILE FILES %%%

% compile_files/2 - Given a list of code wildcards and compile options,
% compile all matching files.
compile_files(Code, CompileOptions) ->
	F = fun(File) -> 
		io:format("Compiling: ~s~n", [File]),
		{ok, _} = compile:file(File, CompileOptions)
	end,
	[F(X) || X <- get_files(Code)],
	ok.

%%% MIRRORING %%%

% mirror/3 - Given a list of Nodes, a list of wildcards to mirror,
% and a list of wildcards to ignore, make it so that the nodes all have
% the same files as the local server.
mirror([], _, _) -> ok;
mirror(Nodes, Mirror, Ignore) ->
	Files = get_files(Mirror) -- get_files(Ignore),
	mirror_files(Nodes, Files),
	delete_extra_files(Nodes, Files, Mirror, Ignore),
	ok.


% mirror_files/2 - Given a list of nodes and a list of files,
% make sure that all of the nodes have the latest version of the files.
% Does this by comparing MD5's of the file. 
% This should probably not be used with huge files.
mirror_files(Nodes, Files) ->
	LocalMD5s = get_md5s(Files),
	[mirror_files_on_node(X, Files, LocalMD5s) || X <- Nodes],
	ok.
	
% mirror_files_on_node/3 - Given a node, a list of files, and the corresponding
% MD5s for the local files, compare the remote MD5, and if it doesn't
% match than write the file to the remote machine.
mirror_files_on_node(Node, Files, LocalMD5s) ->
	% Get remote MD5s...
	F1 = fun() -> get_md5s(Files) end,
	RemoteMD5s = rpc:call(Node, ?MODULE, remote, [F1]),
	
	F2 = fun(N) ->
		case [lists:nth(N, LocalMD5s), lists:nth(N, RemoteMD5s)] of			
			[{_, MD5}, {_, MD5}] -> ok;
			[{File, _LocalMD5}, {_, _RemoteMD5}]-> mirror_file_on_node(Node, File)
		end
	end,
	[F2(X) || X <- lists:seq(1, length(Files))],
	ok.	

% mirror_file_on_node/2 - By this point, we've decided we should
% copy the file to the remote machine. This function does that.
mirror_file_on_node(Node, File) ->
	io:format("Mirroring: ~s~n", [File]),
	{ok, B} = file:read_file(File),
	F = fun() -> 
		ok = filelib:ensure_dir(File),
		ok = file:write_file(File, B),
		ok
	end,
	ok = rpc:call(Node, ?MODULE, remote, [F]),
	ok.
	

% delete_extra_files/4 - Given a list of nodes, a list of files, 
% a list of wildcards to mirror, and a list of wildcards to ignore,
% remove any extraneous files on the supplied nodes that are not found
% in the list of files.
delete_extra_files([], _, _, _) -> ok;
delete_extra_files([Node|T], Files, Mirror, Ignore) ->
	F = fun() ->
		ExtraFiles = (get_files(Mirror) -- get_files(Ignore)) -- Files,
		[file:delete(X) || X <- ExtraFiles],
		ok
	end,
	ok = rpc:call(Node, ?MODULE, remote, [F]),
	delete_extra_files(T, Files, Mirror, Ignore).
	
	
	
%%% START APPLICATIONS %%%

start([]) -> ok;
start([{Nodes, Application, Args}|Apps]) ->
	F = fun() ->
		io:format("Loading application: ~s~n", [Application]),
		% Load the app.
		case application:load(Application) of
			ok -> ok;
			{error, {already_loaded, _}} -> ok;
			Other1 -> throw(Other1)
		end,
		
		% Set properties.
		[ok=application:set_env(Application, Key, Value) || {Key, Value} <- Args],
		
		% Start the app.
		io:format("Starting application: ~s~n", [Application]),
		case application:start(Application) of
			ok -> ok;
			{error, {already_started, _}} -> ok;
			Other2 -> throw(Other2)
		end,		
		ok
	end,
	
	case rpc:multicall(Nodes, ?MODULE, remote, [F]) of
		{_, []} -> ok;
		{_, BadNodes} -> throw(BadNodes)
	end,
	
	start(Apps).


	
%%% PRIVATE FUNCTIONS %%%

get_environment() ->
	case init:get_argument(sync_environment) of
		{ok, [[L]]} -> list_to_atom(L);
		_ -> development
	end.


% remote/1 - Execute the supplied function remotely.
% Used as a helper for RPC calls.
remote(Function) -> Function().
	
	
% get_files/1 - Given the specified wildcards, return a list of files.
get_files([]) -> [];
get_files([H|T]) ->
	Files2 = get_files1(filelib:wildcard(H)),
	Files2 ++ get_files(T).

get_files1([]) -> [];
get_files1([H|T]) ->
	case filelib:is_dir(H) of
		true -> 
			Path = filename:join(H, "*"),
			L = get_files1(filelib:wildcard(Path)),
			L ++ get_files1(T);
			
		false ->
			[H|get_files1(T)]
	end.

% get_md5s/1 - Given a list of files, return a list of {File, MD5} tuples.	
get_md5s([]) -> [];
get_md5s([H|T]) ->
	case file:read_file(H) of
		{ok, B} -> [{H, erlang:md5(B)}|get_md5s(T)];
		_ -> [{H, undefined}|get_md5s(T)]
	end.