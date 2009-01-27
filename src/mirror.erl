% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.
%
%
% Mirror allows you to transfer files to remote nodes from within Erlang. 
% All files and paths must be relative, and under the current directory.
%
% MIRRORING FILES:
% mirror:file(Nodes, Files)
% - Nodes -> node or [node]
% - Files -> "file" or ["file", "file"]
%
% MIRRORING A DIRECTORY:
% mirror:dir(Nodes, Dir, IgnoreList)
%	- Nodes      -> node or [node]
% - Dir        -> "." or "priv"
% - IgnoreList -> "./ebin" or ["ebin", "scratch"]
%

-module(mirror).
-export ([
	file/2,
	dir/3,
	
	% Private
	remote/1
]).

file(Nodes, File) -> 
	mirror(Nodes, [File]),
	ok.

dir(Nodes, Dir, IgnoreList) ->
	Files = get_files(Dir, IgnoreList),
	mirror(Nodes, Files),
	delete_extra_files(Nodes, Files, Dir, IgnoreList),
	ok.


%%% MIRRORING %%%

% mirror_files/2 - Given a list of nodes and a list of files,
% make sure that all of the nodes have the latest version of the files.
% Does this by comparing MD5's of the file. 
% This should probably not be used with huge files.
mirror(Nodes, Files) ->
	Nodes1 = normalize_nodes(Nodes),

	% Connect to nodes, make sure mirror file is out there.
	[pong = net_adm:ping(X) || X <- Nodes1],
	c:nl(?MODULE),
	
	% Get local md5s, and start the mirroring... 
	LocalMD5s = get_md5s(Files),
	[mirror_files_on_node(X, Files, LocalMD5s) || X <- Nodes1],
	ok.
	
% mirror_files_on_node/3 - Given a node, a list of files, and the corresponding
% MD5s for the local files, compare the remote MD5, and if it doesn't
% match than write the file to the remote machine.
mirror_files_on_node(Node, Files, LocalMD5s) ->
	io:format("Mirroring Files on ~s:~n", [Node]),
	% Get remote MD5s...
	F1 = fun() -> get_md5s(Files) end,
	RemoteMD5s = rpc:call(Node, ?MODULE, remote, [F1]),
	
	% Compare local and remote MD5s. If different, then 
	% copy the file to the remote node...
	F2 = fun(N) ->
		case [lists:nth(N, LocalMD5s), lists:nth(N, RemoteMD5s)] of			
			[{_, MD5}, {_, MD5}] -> ok;
			[{File, _LocalMD5}, {_, _RemoteMD5}]-> mirror_file_on_node(Node, File)
		end
	end,
	[F2(X) || X <- lists:seq(1, length(Files))],
	io:format(" - Done.~n~n"),
	ok.	

% mirror_file_on_node/2 - By this point, we've decided we should
% copy the file to the remote machine. This function does that.
mirror_file_on_node(Node, File) ->
	io:format(" - Mirroring File: ~s~n", [File]),
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
delete_extra_files(Nodes, Files, Dir, IgnoreList) ->
	Nodes1 = normalize_nodes(Nodes),

	F = fun() ->
		io:format("Deleting Extra Files on ~s:~n", [node()]),
		ExtraFiles = get_files(Dir, IgnoreList) -- Files,
		[io:format(" - Removing File: ~p~n", [X]) || X <- ExtraFiles],
		[file:delete(X) || X <- ExtraFiles],
		io:format(" - Done.~n~n"),
		ok
	end,
	[ok=rpc:call(Node, ?MODULE, remote, [F]) || Node <- Nodes1],
	ok.
	
	
	
%%% PRIVATE FUNCTIONS %%%

%% normalize_nodes/1 - Convert the specified nodes
%% to a flat list.
normalize_nodes(Nodes) -> lists:flatten([Nodes]).
	
	
%% normalize_files/1 - Convert a possibly deep list of files/directories/or wildcards to
%% a flat list, and then normalize to a local path.
normalize_files([]) -> [];
normalize_files([H|T]) when is_list(H) -> normalize_files(H) ++ normalize_files(T);
normalize_files([H|T]) when is_integer(H) -> [normalize_file([H|T])].
	
	
%% normalize_file/1 - Convert file to a relative path starting with ./.
%% If it's not a relative path under the current directory, then error!
normalize_file("./" ++ File) -> normalize_file(File);
normalize_file(File) -> 
	case string:str(File, "..") of
		0 -> "./" ++ File;
		_ -> throw({invalid_filename, File})
	end.	


% remote/1 - Execute the supplied function remotely.
% Used as a helper for RPC calls.
remote(Function) -> Function().
	

% get_files/1 - Given a directory and a list of paths to ignore, return a list of files.
get_files(Dir, IgnoreList) -> 
	Dir1 = normalize_file(Dir),
	IgnoreList1 = normalize_files(IgnoreList),
	F = fun(F, Acc) ->
		F1 = normalize_file(F),
		case is_ignored(F1, IgnoreList1) of
			true -> Acc;
			false -> [F1|Acc]
		end
	end,
	filelib:fold_files(Dir1, ".*", true, F, []).

is_ignored(_, []) -> false;
is_ignored(File, [H|T]) ->
	case string:str(File, H) of
		0 -> is_ignored(File, T);
		_ -> true
	end.

% get_md5s/1 - Given a list of files, return a list of {File, MD5} tuples.	
get_md5s([]) -> [];
get_md5s([H|T]) ->
	case file:read_file(H) of
		{ok, B} -> [{H, erlang:md5(B)}|get_md5s(T)];
		_ -> [{H, undefined}|get_md5s(T)]
	end.