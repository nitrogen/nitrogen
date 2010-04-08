% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (sync).
-export ([go/0, go/1]).
-include_lib("kernel/include/file.hrl").
-include_lib ("wf.hrl").
-compile(export_all).



%% @doc
%% Sync is a utility to help you hot-load changed code in an Erlang system.
%% When you run sync:go(), sync will recursively find all Emakefiles under the current directory,
%% plus all relative code paths, and call make:all([load]) for each one.


%% @doc
%% Same as sync:go([load]).
go() ->
    go([load]).


%% @doc
%% The specified Options are passed into make:all/1.
go(Options) -> 
    {ok, BaseDir} = file:get_cwd(),
    SubDirs = get_emakefiles_subdirs(),
    CodePathDirs = get_emakefiles_codepaths(),
    Directories = (SubDirs -- CodePathDirs) ++ CodePathDirs,
    run_make_all(BaseDir, Directories, Options).


%% Get directories containing Emakefiles under the current
%% directory...
get_emakefiles_subdirs() ->
    %% Get a reference to all makefile locations...
    Locations = [".", "*", "*/*", "*/*/*", "*/*/*/*", "*/*/*/*/*"],
    F = fun(X, Acc) ->
        filelib:wildcard(X ++ "/Emakefile") ++ Acc
    end,
    Files = lists:foldl(F, [], Locations),

    %% Convert locations to directories...
    [filename:dirname(X) || X <- Files].

%% Get directories possibly containing Emakefiles referenced in the
%% codepath.
get_emakefiles_codepaths() ->
    F = fun(X, Acc) ->
        case normalize_codepath(X) of
            {ok, Path} -> [Path|Acc];
            undefined  -> Acc
        end
    end,
    lists:foldl(F, [], code:get_path()).

normalize_codepath([$/|_]) -> undefined;
normalize_codepath(X) ->
    case lists:reverse(X) of
        "nibe/" ++ L -> {ok, lists:reverse(L)};
        _ -> undefined
    end.

%% @private
%% Loop through each directory. If an Emakefile is found
%% then change to that directory and run make:all()
run_make_all(BaseDir, Dirs, Options) ->
    case inner_run_make_all(BaseDir, Dirs, Options) of
        up_to_date ->
            file:set_cwd(BaseDir),
            io:format(" :: Done!~n"),
            ok;
        error ->
            file:set_cwd(BaseDir),
            io:format(" :: Errors!~n"),
            error
    end.

inner_run_make_all(_BaseDir, [], _Options) -> 
    up_to_date;
inner_run_make_all(BaseDir, [Dir|Dirs], Options) ->
    file:set_cwd(filename:join(BaseDir, Dir)),
    case filelib:is_file("./Emakefile") of
        true ->
            io:format(":: MAKE - ~s~n", [Dir]),
            case make:all(Options) of
                up_to_date -> 
                    inner_run_make_all(BaseDir, Dirs, Options);
                error -> 
                    error
            end;
        false ->
            inner_run_make_all(BaseDir, Dirs, Options)
    end.
