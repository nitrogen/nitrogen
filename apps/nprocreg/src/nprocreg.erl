% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

%%% nprocreg is a global process registry built for Nitrogen. The goal
%%% of nprocreg is to allow Key-based lookups of a Pid, and if no Pid
%%% is found, to start a new process based on a provided Function,
%%% load-balanced newly spawned functions across the cluster.
%%%
%%% The nprocreg gen_server, when run on a node in a Nitrogen cluster,
%%% will automatically connect to other nprocreg servers in other
%%% nodes in the cluster.
%%%
%%% Nodes discover eachother by broadcasting out their existence to
%%% all other nodes in the cluster, retrieved by nodes(). When a node
%%% receives a message from another node, it updates a state variable,
%%% tracking the node. If enough time has passed since the last
%%% checkin, the node is removed, because we assume that the
%%% application has stopped. (Note that the node itself might still be
%%% available, we ignore this fact.)
%%%
%%% The case that we must be careful of is when two processes look up
%%% the same Key at the same time on different nodes, potentially
%%% leading multiple Pids associated with the same key. To avoid this
%%% problem, we hash the Key to a node, and try to start the process
%%% on that node. This effectively makes process creation single
%%% threaded, drastically reducing the opportunity of creating
%%% conflicting Pids. Note that this opportunity still exists: when a
%%% new node running nprocreg is started, or an existing one is
%%% stopped, different nodes could have a different view of which
%%% nodes are available. This will happen so infrequently, and for
%%% such a short time, that we just ignore it.

-module (nprocreg).
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
-define(TABLE, ?MODULE).
-define(COLLECT_TIMEOUT, timer:seconds(2)).
-define(NODE_CHATTER_INTERVAL, timer:seconds(5)).
-define(NODE_TIMEOUT, timer:seconds(10)).
-define(PRINT(Var), error_logger:info_msg("DEBUG: ~p:~p~n~p~n  ~p~n", [?MODULE, ?LINE, ??Var, Var])).
-record(state, { nodes=[], pids=[] }).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get_pid(Key) ->
    Node = gen_server:call(?SERVER, {get_node, Key}),
    gen_server:call({?SERVER, Node}, {get_pid, Key, undefined}).

get_pid(Key, Function) ->
    Node = gen_server:call(?SERVER, {get_node, Key}),
    gen_server:call({?SERVER, Node}, {get_pid, Key, Function}).

init(_) -> 
    % Detect when a process goes down so that we can remove it from
    % the registry.
    process_flag(trap_exit, true),

    %% Broadcast to all nodes at intervals...
    gen_server:cast(?SERVER, broadcast_node),
    timer:apply_interval(?NODE_CHATTER_INTERVAL, gen_server, cast, [?SERVER, broadcast_node]),
    {ok, #state{ nodes=[{node(), never_expire}] }}.

handle_call({get_node, Key}, _From, State) ->
    %% Get the list of nodes that are alive, sorted in ascending order...
    Nodes = lists:sort([Node || {Node, _} <- State#state.nodes, net_adm:ping(Node) == pong]),

    %% Get an MD5 of the Key...
    <<Int:128/integer>> = erlang:md5(term_to_binary(Key)),

    %% Hash to a node...
    N = (Int rem length(Nodes)) + 1,
    Node = lists:nth(N, Nodes),
    {reply, Node, State};

handle_call({get_pid, Key, Function}, _From, State) ->
    %% Try to get the pid locally first. If that doesn't work, then
    %% try to get the pid from one of the other nodes. If we don't
    %% find anything and is_function(Function) == true, then spawn off
    %% a new function on the current node.
    case get_pid_local(Key, State) of
        {ok, LocalPid} ->
            {reply, LocalPid, State};

        undefined ->
            case get_pid_remote(Key, State) of
                {ok, RemotePid} ->
                    {reply, RemotePid, State};

                undefined when Function == undefined ->
                    {reply, undefined, State};

                undefined when is_function(Function) ->
                    {NewPid, NewState} = start_function(Key, Function, State),
                    {reply, NewPid, NewState} 
            end
    end;

handle_call(Message, _From, _State) ->
    throw({unhandled_call, Message}).

handle_cast({get_pid, Key, Pid, Ref}, State) ->
    %% This is called by get_pid_remote. Send back a message with the
    %% Pid if we have it.
    case get_pid_local(Key, State) of
        {ok, LocalPid} ->
            Pid!{get_pid_response, LocalPid, Ref};
        undefined ->
            Pid!{get_pid_response, undefined, Ref}
    end,
    {noreply, State};

handle_cast({register_node, Node}, State) ->
    %% Register that we heard from a node. Set the last checkin time to now().
    Nodes = State#state.nodes,
    NewNodes = lists:keystore(Node, 1, Nodes, {Node, now()}),
    NewState = State#state { nodes=NewNodes },
    {noreply, NewState};

handle_cast(broadcast_node, State) ->
    %% Remove any nodes that haven't contacted us in a while...
    F = fun({_Node, LastContact}) ->
        (LastContact == never_expire) orelse
        (timer:now_diff(now(), LastContact) / 1000) < ?NODE_TIMEOUT
    end,
    NewNodes = lists:filter(F, State#state.nodes),

    %% Alert all nodes that we are here...
    gen_server:abcast(nodes(), ?SERVER, {register_node, node()}),
    {noreply, State#state { nodes=NewNodes }};

%% @private
handle_cast(Message, _State) -> 
    throw({unhandled_cast, Message}).

%% @private
handle_info({'EXIT', Pid, _Reason}, State) ->
    %% A process died, so remove it from our list of pids.
    NewPids = lists:keydelete(Pid, 2, State#state.pids),
    {noreply, State#state { pids=NewPids }};

handle_info(_Message, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) -> ok.

%% @private
code_change(_OldVsn, State, _Extra) -> {ok, State}.

get_pid_local(Key, State) ->
    %% Return the pid if it exists.
    case lists:keyfind(Key, 1, State#state.pids) of
        {Key, Pid} ->
            {ok, Pid};
        false ->
            undefined
    end.

get_pid_remote(Key, State) ->
    %% Ask the other nodes for the pid, collect responses.
    Nodes = [X || {X, _} <- State#state.nodes, X /= node()],
    Ref = make_ref(),
    gen_server:abcast(Nodes, ?SERVER, {get_pid, Key, self(), Ref}),
    get_pid_remote_collect(Ref, State, length(Nodes)).

get_pid_remote_collect(_, _State, 0) ->
    undefined;
get_pid_remote_collect(Ref, State, RepliesRemaining) ->
    receive
        {get_pid_response, Pid, Ref} when is_pid(Pid) ->
            {ok, Pid};
        {get_pid_response, undefined, Ref} ->
            get_pid_remote_collect(Ref, State, RepliesRemaining - 1)
    after ?COLLECT_TIMEOUT ->
        undefined
    end.

start_function(Key, Function, State) ->
    %% Create the function, register locally.
    Pid = erlang:spawn_link(Function),
    NewPids = [{Key, Pid}|State#state.pids],
    NewState = State#state { pids=NewPids },
    {Pid, NewState}.
    
