-module (sync_configuration).
-include ("wf.inc").
-export ([get_node_types/1, get_nodes/2, start_application/3]).

%% Configuration file for the Nitrogen 'sync' utility.
%%
%% Sync helps you compile code and dispatch it to remote nodes, complete
%% with different nodes for a development, test, and production environment.
%%
%% Some sync commands:
%%
%%   sync:local() - Recompile and reload any changed modules locally.
%%   sync:force() - Recompile and reload ALL modules locally.
%%   sync:soft()  - Reload any changed modules across all remote nodes.
%%   sync:hard()  - Reboot all nodes, and then push new modules to all nodes.
%%   sync:go()    - Do a sync:warm(), then start all applications.
%%
%% To configure sync, add more clauses to the functions defined below.

%% get_node_types/1 - 
%% Given a specific environment, return a list of all applications that should run.
get_node_types(_Environment) -> [crypto, mnesia, inets, timer, inets_helper].

%% get_nodes/2 - 
%% Given an environment and an application, return a list of nodes on which
%% that application should be running.
get_nodes(_Environment, _Application) -> [node()].

%% start_application/3 - 
%% Given an environment, node, and application, start the application
%% on the specified node.
start_application(_Environment, Node, Application) ->
	rpc:call(Node, erlang, system_flag, [fullsweep_after, 0]),
	sync:start_if_not_running(Node, Application).
	
