% Nitrogen Web Framework for Erlang
% Copyright (c) 2008 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (sync).
-behaviour (supervisor).
-compile(export_all).
-export ([
	%% Supervisor
	go/0,
	start/0,
	start/2,
	stop/1,
	init/1,
	start_pulse/1,
	pulse/1,

	%% Node Management
	connect/0,

	%% Code Synchronization
	local/0,
	force/0,
	soft/0,
	hard/0,
	get_codefiles/0,
	
	%% Utils
	get_environment/0,
	distribute_tables/2,
	is_running/2,
	start_if_not_running/2
]).

start() -> application:start(sync).
start(_Type, _Arg) -> supervisor:start_link(?MODULE, []).
stop(_State) -> ok.
	
init(_Args) ->
	% Return the supervisor configuration...	
	{ok, {{one_for_one, 1, 60}, [
		{sync_pulse, {sync, start_pulse, [10]}, permanent, brutal_kill, worker, [sync]}
	]}}.
	
start_pulse(Seconds) -> 
	Pid = spawn_link(fun() -> pulse(Seconds) end),
	erlang:register(sync_pulse, Pid),
	{ok, Pid}.
	
pulse(Seconds) ->
	pulse_code(),
	pulse_apps(),
	
	case is_integer(Seconds) of
		true ->
			Timeout = Seconds * 1000,
			receive after Timeout -> ok end,
			pulse(Seconds);
		false ->
			ok
	end.

pulse_code() ->
	% Get a list of all nodes and share the db to them.
	sync:connect(),
	sync:soft(),
	ok.
	
pulse_apps() ->
	% Distribute the database...
	mnesia:start(),
	mnesia:change_config(extra_db_nodes, nodes()),

	% Start applications on all nodes...
	Environment = get_environment(),
	F = fun(NodeType) ->
		io:format("Starting ~w nodes...~n", [NodeType]),
		[sync_configuration:start_application(Environment, X, NodeType) || X <- sync_configuration:get_nodes(Environment, NodeType)]
	end,		
	[F(X) || X <- sync_configuration:get_node_types(Environment)],
	ok.
	
go() -> pulse(once).

%%% NODE MANAGEMENT %%%

get_environment() ->
	{ok, [[L]]} = init:get_argument(sync_environment),
	list_to_atom(L).
	
connect() -> 
  Environment = get_environment(),
	F = fun(NodeType) -> [pong = net_adm:ping(X) || X <- sync_configuration:get_nodes(Environment, NodeType)] end,
	[F(X) || X <- sync_configuration:get_node_types(Environment)],
	nodes().

%%% CODE SYNCHRONIZATION %%%

compile(SrcFile) ->
	Options = [verbose, debug_info, report_errors, report_warnings, {outdir, "./ebin/"}],
	IncludePaths = [{i, "./src/include/"}] ++ [{i, X} || X <- code:get_path()],
	io:format("Compiling: ~s~n", [SrcFile]),
	{ok, ModuleName } = compile:file(SrcFile, Options ++ IncludePaths),
	c:l(ModuleName).
	

%% force/0 - Reload and recompile all files locally.
force() ->
	CodeFiles = get_codefiles(),
	% Compile and load the files locally...
	[compile(X) || X <- CodeFiles],
	io:format("Done with forced recompile!~n"),
	ok.
	
	
local() ->
	% Get all files in the ./src directory that have changed...
	ChangedFiles = get_changed_codefiles(),

	% Compile and load the files locally...
	[compile(X) || X <- ChangedFiles],
	io:format("Done compiling locally!~n"),
	ok.
	
%% soft/0 - Update modules on all nodes. Doesn't affect up-to-date modules.
soft() ->
	% Get a list of all files in ./src...
	ok = local(),
	F1 = fun(X, Acc) -> [get_modulename(X)|Acc] end,
	AllModules = filelib:fold_files("./src", ".*.erl", true, F1, []),
	
	F2 = fun(Node, Module) ->
		case is_module_old(Node, Module) of
			true ->
				io:format("Reloading ~w on ~w.~n", [Module, Node]),
				{Module, Binary, Filename} = code:get_object_code(Module),
				{module, _} = rpc:call(Node, code, load_binary, [Module, Filename, Binary]),
				ok;
			false ->
				ok
		end
	end,

	[F2(N, M) || N <- nodes(), M <- AllModules],
	io:format("Done with soft sync!~n"),
	ok.
	
%% hard/0 - Restart nodes, then reload all modules on them.
hard() ->
	% Restart all nodes...
	Nodes = nodes(),
	F1 = fun(Node) -> rpc:cast(Node, init, restart, []) end,
	[F1(X) || X <- Nodes],
	io:format("Waiting 10 seconds for nodes to cycle...~n"),	
	timer:sleep(10 * 1000),
		
	% Connect to the nodes...
	reconnect_nodes(Nodes),
	
	% Get a list of all files in ./src...
	ok = local(),
	F = fun(X, Acc) -> [get_modulename(X)|Acc] end,
	AllModules = filelib:fold_files("./src", ".*.erl", true, F, []),
	
	% Reload all modules...
	[c:nl(X) || X <- AllModules],
	io:format("Reloaded all modules on nodes: ~p~n", [nodes()]),
	io:format("Done with hard sync!~n"),
	ok.

%% get_modulename/1 - Convert a source file path 
%% into a module name.
get_modulename(SrcFile) -> 
	list_to_atom(filename:basename(filename:rootname(SrcFile))).

%% get_codefiles/0 - Get a list of all
%% code files on the current node.
get_codefiles() ->
	F = fun(SrcFile, Acc) -> [SrcFile|Acc] end,
	filelib:fold_files("./src", ".*.erl", true, F, []).
	
%% get_priv_directories/0 - Get a list of all directories under priv. Look for includes in here.
get_priv_directories() ->
	{ok, L1} = file:list_dir("priv"),
	L2 = [{X, file:read_file_info("priv/" ++ X)} || X <- L1],
	[Path || {Path, {ok, Info}} <- L2, element(3, Info) == directory].

%% get_changed_codefiles/0 - Get a list of all
%% code files that have changed on the current node for
%% local compilation.
get_changed_codefiles() ->
	F = fun(SrcFile, Acc) -> 
		case has_code_changed(SrcFile) of 
			true -> [SrcFile|Acc];
			false -> Acc
		end
	end,
	filelib:fold_files("./src", ".*.erl", true, F, []).

%% has_code_changed/1 - Check if the code in SrcFile is newer
%% than the module currently loaded.
has_code_changed(SrcFile) ->
	ModuleName = get_modulename(SrcFile),
	ModuleTime = get_moduletime(node(), ModuleName),
	SrcFileTime = filelib:last_modified(SrcFile),
	(ModuleTime < SrcFileTime).
	
%% is_module_old/2 - Check if the specified Module on the local
%% node is newer than the code on the specified Node.
is_module_old(Node, Module) ->
	(get_moduletime(Node, Module) < get_moduletime(node(), Module)).	
	
get_moduletime(Node, Module) ->
	case rpc:call(Node, code, ensure_loaded, [Module]) of
		{error, _} -> {{0, 0, 0}, {0, 0, 0}};
		_ ->
			ModuleInfo = rpc:call(Node, Module, module_info, [compile]),
			{value, {time, {Year, Month, Day, Hour, Minute, Second}}} = lists:keysearch(time, 1, ModuleInfo),
			calendar:universal_time_to_local_time({{Year, Month, Day}, {Hour, Minute, Second}})
	end.

reconnect_nodes(Nodes) -> reconnect_nodes(Nodes, 0).
reconnect_nodes([], _) -> ok;
reconnect_nodes([Node|Nodes], Count) ->
	io:format("Connecting to ~w...~n", [Node]),
	case net_adm:ping(Node) of
		pong -> reconnect_nodes(Nodes, 0);
		pang when Count =< 15 -> 
			timer:sleep(1000),
			reconnect_nodes([Node|Nodes], Count + 1);
		pang ->
			throw({node_down, Node})
	end.	
	
distribute_tables(Node, Tables) ->
	F = fun(Table) -> mnesia:add_table_copy(Table, Node, ram_copies) end,
	[F(X) || X <- Tables],
	ok.

is_running(Node, Application) ->
	Apps = rpc:call(Node, application, which_applications, []),
	lists:keymember(Application, 1, Apps).
	
start_if_not_running(Node, Application) ->
	ok = case is_running(Node, Application) of
		true -> ok;
		false -> 
			case rpc:call(Node, application, start, [Application]) of
				{error,_} -> Application:start();
				_ -> ok
			end
	end.