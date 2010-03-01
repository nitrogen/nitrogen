-module (mprocreg).
-behaviour (gen_server).

-export([
    start_link/0,
    get_pid/1,
    get_pid/2,
    init/1,
    handle_cast/2,
    handle_call/3,
    handle_info/2,
    code_change/3,
    terminate/2
]).

-define(SERVER, ?MODULE).
-define(TABLE, process_registry).
-define(SCAN_INTERVAL, timer:seconds(60)).
-define(PRINT(Var), error_logger:info_msg("DEBUG: ~p:~p~n~p~n  ~p~n", [?MODULE, ?LINE, ??Var, Var])).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_pid(Key) ->
    gen_server:call(?MODULE, {get_pid, Key}).

get_pid(Key, Function) ->
    gen_server:call(?MODULE, {get_pid, Key, Function}).

init(_) -> 
    % Detect when a process goes down so that we can remove it from
    % the registry.
    process_flag(trap_exit, true),

    % Ensure mnesia is started and connected.
    ok = mnesia:start(),
    case application:get_env(nodes) of
        {ok, Nodes} when is_list(Nodes) -> 
            error_logger:info_msg("Starting process registry on ~p~n.", [Nodes]),
            mnesia:change_config(extra_db_nodes, Nodes);
        _ -> 
            M = "Starting process registry on local node. ({mprocreg, nodes} is not set!)~n",
            error_logger:info_msg(M)

    end,

    % Try to create the table, if successful, then we're fine.  If
    % not, then some other node has it, so create a copy of it
    % locally.
    case mnesia:create_table(?TABLE, []) of
        {atomic, ok} -> 
            mnesia:add_table_index(?TABLE, val),
            ok;
        _Other -> 
            mnesia:add_table_copy(?TABLE, node(), ram_copies)
    end,
    {ok, ignored}.

handle_call({get_pid, Key}, _From, State) ->
    Pid = case mnesia:dirty_read({?TABLE, Key}) of
        [{?TABLE, Key, Value}] -> Value;
        [] -> undefined
    end,
    IsPidAlive = (
        Pid /= undefined andalso 
        is_pid(Pid) andalso 
        rpc:call(node(Pid), erlang, is_process_alive, [Pid])
    ),
    case IsPidAlive of
        true  -> {reply, Pid, State};
        false -> {reply, undefined, State}
    end;

handle_call({get_pid, Key, Function}, _From, State) ->
    {reply, Pid, NewState} = handle_call({get_pid, Key}, ignored, State),
    Pid1 = case Pid/=undefined of
        true -> Pid;
        false ->
            NewPid = erlang:spawn_link(Function),
            mnesia:dirty_write({?TABLE, Key, NewPid}),
            NewPid
    end,
    {reply, Pid1, NewState};

handle_call(Message, _From, _State) ->
    throw({unhandled_call, Message}).

%% @private
handle_cast(Message, _State) -> 
    throw({unhandled_cast, Message}).

%% @private
handle_info({'EXIT', Pid, _Reason}, State) ->
    case mnesia:dirty_index_read(?TABLE, Pid, 3) of
        [{?TABLE, Key, Pid}] ->
            mnesia:dirty_delete(?TABLE, Key);
        Other ->
            ?PRINT({result_unexpected, Other})
    end,
    {noreply, State};

handle_info(Message, _State) ->
    throw({unhandled_info, Message}).

%% @private
terminate(_Reason, _State) -> ok.

%% @private
code_change(_OldVsn, State, _Extra) -> {ok, State}.
