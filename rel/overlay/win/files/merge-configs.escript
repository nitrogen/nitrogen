#!/usr/bin/env escript
%% vim: sw=4 ts=4 et ft=erlang

%% This will merge the files matching ./etc/*.config and merge them into a single app.config, removing the source files, this assumes that those files are proplists. This is way to work around the fact that werl doesn't accept multiple -config files, apparently.

-define(P(X),io:format("~s~n",[X])).

main([BaseDir]) ->
    LastChar = lists:last(BaseDir),
    if
        LastChar == $/;
        LastChar == $\ ->
            NewBaseDir = lists:sublist(BaseDir,length(BaseDir)-1),
            main([NewBaseDir]);
        true -> do_main(BaseDir)
    end;
main(_) ->
    ?P("Usage ./merge-configs.escript <ConfigDirectory>"),
    ?P("Example: ./merge-configs.escript ./etc").

do_main(BaseDir) ->
    TargetFile = BaseDir ++ "/app.generated.config",
    Files = get_file_list(BaseDir,TargetFile),
    Config = build_config(Files),
    write_generated(Config,TargetFile),
    %delete_configs(Files),
    %finalize_config(),
    ok.

get_file_list(BaseDir,TargetFile) ->
    ?P("Compiling list of .config files"),
    Files = filelib:wildcard(BaseDir ++ "/*.config"),
    %% Windows is case insensitive - so let's just make everything lowercase
    TargetFileBase = string:to_lower(filename:basename(TargetFile)),

    %% We don't want to read the TargetFile, since this is what will be generated, so let's remove it from the response
    lists:filter(fun(F) ->
        string:to_lower(filename:basename(F))=/=TargetFileBase
    end, Files).

build_config(Files) ->
    ?P("Loading and appending Configuration files..."),
    NewConfig = lists:foldl(fun(File,AccConf) ->
        ?P("..." ++ File),
        {ok,ThisConf} = file:consult(File),
        ThisConf ++ AccConf
    end,[],Files),
    lists:flatten(NewConfig).

write_generated(Config,TargetFile) ->
    ?P("Writing generated config file"),
    file:write_file(TargetFile,io_lib:fwrite("~p.\n",[Config])).

%% finalize_config() ->
%%     ?P("Moving temporary config file to app.config"),
%%     file:rename("./etc/app.config.temp","./etc/app.config").
%% 
%% delete_configs(Files) ->
%%     ?P("Deleteing config files..."),
%%     lists:foreach(fun(F) ->
%%         ?P("..." ++ F),
%%         ok = file:delete(F)
%%     end,Files).
