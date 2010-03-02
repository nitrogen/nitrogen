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
%% When you run sync:go(), sync will:
%% 1. Find all loaded modules that are not system modules.
%% 2. Search their directories for an Emakefile.
%% 3. If an Emakefile is found, change to the directory and run make:all([load]).



%% @doc
%% Same as sync:go([load]).
go() ->
    go([load]).



%% @doc
%% The specified Options are passed into make:all/1.
go(Options) -> 
    UserModules = get_user_modules(),
    Directories = extract_directories(UserModules),
    run_make_all(Directories, Options).



%% @private
%% Get all loaded modules that are NOT system modules.
get_user_modules() ->
    % Get a list of all loaded modules...
    Modules = [X || {X, _} <- code:all_loaded()],

    % Filter out sticky modules...
    F1 = fun(X) -> code:is_sticky(X) == false end,
    Modules1 = lists:filter(F1, Modules),	

    % Filter out system modules...
    F2 = fun(X, Acc) ->
        case application:get_key(X, modules) of
            {ok, L} -> L ++ Acc;
            undefined -> Acc
        end
    end,
    SystemModules = lists:foldl(F2, [], system_applications()),
    Modules1 -- SystemModules.



%% @private
%% Given a list of modules, pull out the src
%% directory plus one level up.
extract_directories(Modules) ->
    F = fun(Module, Acc) ->
        % Add the src dir plus one level up...
        case get_src_dir(Module) of
            undefined -> Acc;
            SrcDir ->
                SrcDirs = sets:from_list([SrcDir, filename:dirname(SrcDir)]),
                sets:union(Acc, SrcDirs)
        end
    end,
    Set = lists:foldl(F, sets:new(), Modules),
    lists:sort(sets:to_list(Set)).



%% @private
%% Loop through each directory. If an Emakefile is found
%% then change to that directory and run make:all()
run_make_all(Dirs, Options) ->
    {ok, Cwd} = file:get_cwd(),
    Return = inner_run_make_all(Dirs, Options),
    file:set_cwd(Cwd),
    Return.



inner_run_make_all([], _Options) -> up_to_date;
inner_run_make_all([Dir|Dirs], Options) ->
    FilePath = filename:join(Dir, "Emakefile"),
    case filelib:is_file(FilePath) of
        true ->
            io:format(":: MAKE - ~s~n", [FilePath]),
            file:set_cwd(Dir),
            case make:all(Options) of
                up_to_date -> inner_run_make_all(Dirs, Options);
                error -> error
            end;
        false ->
            inner_run_make_all(Dirs, Options)
    end.



get_src_dir(Module) ->
    case proplists:get_value(source, Module:module_info(compile), undefined) of
        undefined -> undefined;
        Other -> 
            % io:format("~s - ~s~n", [Module, Other]),
            filename:dirname(Other)
    end.



%% @private
%% Return a list of system applications. 
%% Pulled from /usr/local/lib/erlang/lib/*
system_applications() -> [
    appmon,
    asn1,
    common_test,
    compiler,
    cosEvent,
    cosEventDomain,
    cosFileTransfer,
    cosNotification,
    cosProperty,
    cosTime,
    cosTransactions,
    crypto,
    debugger,
    dialyzer,
    docbuilder,
    edoc,
    erl_interface,
    erts,
    et,
    eunit,
    gs,
    hipe,
    ic,
    inets,
    inviso,
    jinterface,
    kernel,
    megaco,
    mnesia,
    observer,
    odbc,
    orber,
    os_mon,
    otp_mibs,
    parsetools,
    percept,
    pman,
    public_key,
    reltool,
    runtime_tools,
    sasl,
    snmp,
    ssh,
    ssl,
    stdlib,
    syntax_tools,
    test_server,
    toolbar,
    tools,
    tv,
    typer,
    webtool,
    wx,
    xmerl
].
