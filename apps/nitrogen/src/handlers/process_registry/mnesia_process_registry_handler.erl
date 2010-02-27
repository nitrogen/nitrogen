% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (mnesia_process_registry_handler).
-behaviour (process_registry_handler).
-include ("wf.inc").
-export ([
    start/0,
    start/1,
    init/2, 
    finish/2,
    get_pid/3,
    get_pid/4,
    remove_dead_processes/0
]).

-define(TABLE, process_registry).
-define(SCAN_INTERVAL, timer:seconds(60)).

start() -> start(nodes()).
start(Nodes) ->
    % Ensure mnesia is started and connected.
    ok = mnesia:start(),
    mnesia:change_config(extra_db_nodes, Nodes),

    % Try to create the table, if successful, then we're fine.
    % If not, then some other node has it, so create a copy of it locally.
    case mnesia:create_table(?TABLE, []) of
        {atomic, ok} -> 
            ok;
        _Other -> 
            mnesia:add_table_copy(?TABLE, node(), ram_copies)
    end,

    % Periodically scan through Mnesia for processes that no longer exist.
    timer:apply_interval(?SCAN_INTERVAL, ?MODULE, remove_dead_processes, []),
    ok.

init(_Config, State) -> 
    {ok, State}.

finish(_Config, State) ->
    {ok, State}.

get_pid(Key, _Config, State) ->
    Pid = case mnesia:dirty_read({?TABLE, Key}) of
        [{?TABLE, Key, Value}] -> Value;
        [] -> undefined
    end,
    IsPidAlive = (
        Pid /= undefined andalso 
        is_pid(Pid) andalso 
        rpc:call(node(Pid), erlang, is_process_alive, [Pid])
    ),
    Pid1 = case IsPidAlive of
        true  -> Pid;
        false -> undefined
    end,
    {ok, Pid1, State}.

get_pid(Key, Function, Config, State) ->
    {ok, Pid, NewState} = get_pid(Key, Config, State),
    Pid1 = case Pid /= undefined of
        true -> Pid;
        false ->
            NewPid = erlang:spawn(Function),
            mnesia:dirty_write({?TABLE, Key, NewPid}),
            NewPid
    end,
    {ok, Pid1, NewState}.

remove_dead_processes() ->
    % Get a list of keys.
    KeyList = mnesia:dirty_all_keys(?TABLE),

    F = fun(Key) ->
        % Get the pid associated with this key.
        case mnesia:dirty_read({?TABLE, Key}) of
            [{?TABLE, Key, Pid}] -> 
                % If the pid is local and it's no longer running, then
                % remove it from mnesia.
                IsLocalAndDead = (
                    is_pid(Pid) andalso 
                    node(Pid) == node() andalso
                    not is_process_alive(Pid)
                ),
                case IsLocalAndDead of
                    true  -> 
                        mnesia:dirty_delete({?TABLE, Key});
                    false -> 
                        ignore
                end; 
            _ ->
                ignore
        end
    end,
    [F(X) || X <- KeyList],
    ok.

