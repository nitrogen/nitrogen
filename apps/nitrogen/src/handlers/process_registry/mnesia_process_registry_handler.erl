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
	get_pid/4
]).

-define (TABLE, process_registry).
-record (entry, {key, value}).

start() -> start(nodes()).
start(Nodes) ->
  % Ensure mnesia is started and connected.
	ok = mnesia:start(),
	mnesia:change_config(extra_db_nodes, Nodes),

  % Try to create the table, if successful, then we're fine.
  % If not, then create a copy of it locally.
  case mnesia:create_table(?TABLE, [{attributes, record_info(fields,entry)}]) of
    {atomic, ok} -> ok;
    _            -> mnesia:add_table_copy(?TABLE, node(), ram_copies)
  end.  

init(_Config, State) -> 
	{ok, State}.

finish(_Config, State) ->
	{ok, State}.
	
get_pid(Key, _Config, State) ->
	Pid = case mnesia:dirty_read({?TABLE, Key}) of
	  [{?TABLE, Key, Value}] -> Value;
	  [] -> undefined
	end,
	IsPidAlive = 
	  Pid /= undefined andalso 
	  is_pid(Pid) andalso 
	  rpc:call(node(Pid), erlang, is_process_alive, [Pid]),
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
	