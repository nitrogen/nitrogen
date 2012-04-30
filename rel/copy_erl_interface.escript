#!/usr/bin/env escript
%% -*- erlang -*-
%% vim: ts=4 sw=4 et ft=erlang
%%
%% Coyright 2012 - Jesse Gumm
%% MIT License
%%
%% The purpoose of this file is due to a behavior or bug in Erlang's reltool
%% functionality, where the erl_interface directory is not copied using reltool.
%% This is a workaround for that shortcoming particularly for building Yaws
%% with Nitrogen, as Yaws requires erl_interface to compile.
%% 	
%% Much of this file has been straight up copied from rebar, most of it is
%% overkill for what it's doing, and can probably be stripped to its bare bones,
%% especially for whenever that shortcoming gets fixed.
%% 
%% So for now, this serves a simple purpose which will likely be rendered 
%% unnecessary in the future.

-define(CONSOLE(Str,Args), io:format(Str,Args)).
-define(FMT(Str,Args), lists:flatten(io_lib:format(Str,Args))).

main([]) ->
    Path = code:lib_dir(erl_interface),
    To = filename:join([get_cwd(), "nitrogen", "lib", filename:basename(Path)]),

    ?CONSOLE("~s~n",["Copying " ++ Path]),
    cp_r([Path],To),
    ok.


%% Copied from Rebar
cp_r(Sources, Dest) ->
    case os:type() of
        {unix, _} ->
            EscSources = [escape_spaces(Src) || Src <- Sources],
            SourceStr = string:join(EscSources, " "),
            {ok, []} = sh(?FMT("cp -R ~s \"~s\"",
                                           [SourceStr, Dest]),
                                      [{use_stdout, false}, return_on_error]),
            ok;
        {win32, _} ->
            lists:foreach(fun(Src) -> ok = cp_r_win32(Src,Dest) end, Sources),
            ok
    end.


%% Windows stuff	
xcopy_win32(Source,Dest)->
    {ok, R} = sh(
                ?FMT("xcopy \"~s\" \"~s\" /q /y /e 2> nul",
                     [filename:nativename(Source), filename:nativename(Dest)]),
                [{use_stdout, false}, return_on_error]),
    case length(R) > 0 of
        %% when xcopy fails, stdout is empty and and error message is printed
        %% to stderr (which is redirected to nul)
        true -> ok;
        false ->
            {error, lists:flatten(
                      io_lib:format("Failed to xcopy from ~s to ~s~n",
                                    [Source, Dest]))}
    end.

cp_r_win32({true, SourceDir}, {true, DestDir}) ->
    %% from directory to directory
    SourceBase = filename:basename(SourceDir),
    ok = case file:make_dir(filename:join(DestDir, SourceBase)) of
             {error, eexist} -> ok;
             Other -> Other
         end,
    ok = xcopy_win32(SourceDir, filename:join(DestDir, SourceBase));
cp_r_win32({false, Source} = S,{true, DestDir}) ->
    %% from file to directory
    cp_r_win32(S, {false, filename:join(DestDir, filename:basename(Source))});
cp_r_win32({false, Source},{false, Dest}) ->
    %% from file to file
    {ok,_} = file:copy(Source, Dest),
    ok;
cp_r_win32({true, SourceDir},{false,DestDir}) -> 
    IsFile = filelib:is_file(DestDir),
    case IsFile of
        true -> 
            %% From Directory to file? This shouldn't happen
            {error,[{rebar_file_utils,cp_r_win32},
                        {directory_to_file_makes_no_sense,
                            {source,SourceDir},{dest,DestDir}}]};
        false ->
            %% Specifying a target directory that doesn't currently exist.
            %% So Let's attempt to create this directory
            %% Will not recursively create parent directories
            ok = case file:make_dir(DestDir) of
                     {error, eexist} -> ok;
                     Other -> Other
                 end,
            ok = xcopy_win32(SourceDir, DestDir)
    end;
cp_r_win32(Source,Dest) ->
    Dst = {filelib:is_dir(Dest), Dest},
    lists:foreach(fun(Src) ->
                          ok = cp_r_win32({filelib:is_dir(Src), Src}, Dst)
                  end, filelib:wildcard(Source)),
    ok.

%% Shell Command Stuff (from rebar_utils)
sh(Command0, Options0) ->
    % ?CONSOLE("sh info:\n\tcwd: ~p\n\tcmd: ~s\n", [get_cwd(), Command0]),
    % ?DEBUG("\topts: ~p\n", [Options0]),

    DefaultOptions = [use_stdout, abort_on_error],
    Options = [expand_sh_flag(V)
               || V <- proplists:compact(Options0 ++ DefaultOptions)],

    ErrorHandler = proplists:get_value(error_handler, Options),
    OutputHandler = proplists:get_value(output_handler, Options),

    Command = patch_on_windows(Command0, proplists:get_value(env, Options, [])),
    PortSettings = proplists:get_all_values(port_settings, Options) ++
        [exit_status, {line, 16384}, use_stdio, stderr_to_stdout, hide],
    Port = open_port({spawn, Command}, PortSettings),

    case sh_loop(Port, OutputHandler, []) of
        {ok, _Output} = Ok ->
            Ok;
        {error, {_Rc, _Output}=Err} ->
            ErrorHandler(Command, Err)
    end.

get_cwd() ->
	{ok,Dir} = file:get_cwd(),
	Dir.

patch_on_windows(Cmd, Env) ->
    case os:type() of
        {win32,nt} ->
            "cmd /q /c "
                ++ lists:foldl(fun({Key, Value}, Acc) ->
                                       expand_env_variable(Acc, Key, Value)
                               end, Cmd, Env);
        _ ->
            Cmd
    end.

expand_env_variable(InStr, VarName, RawVarValue) ->
    case string:chr(InStr, $$) of
        0 ->
            %% No variables to expand
            InStr;
        _ ->
            VarValue = re:replace(RawVarValue, "\\\\", "\\\\\\\\", [global]),
            %% Use a regex to match/replace:
            %% Given variable "FOO": match $FOO\s | $FOOeol | ${FOO}
            RegEx = io_lib:format("\\\$(~s(\\s|$)|{~s})", [VarName, VarName]),
            ReOpts = [global, {return, list}],
            re:replace(InStr, RegEx, [VarValue, "\\2"], ReOpts)
    end.

sh_loop(Port, Fun, Acc) ->
    receive
        {Port, {data, {eol, Line}}} ->
            sh_loop(Port, Fun, Fun(Line ++ "\n", Acc));
        {Port, {data, {noeol, Line}}} ->
            sh_loop(Port, Fun, Fun(Line, Acc));
        {Port, {exit_status, 0}} ->
            {ok, lists:flatten(lists:reverse(Acc))};
        {Port, {exit_status, Rc}} ->
            {error, {Rc, lists:flatten(lists:reverse(Acc))}}
    end.

escape_spaces(Str) ->
    re:replace(Str, " ", "\\\\ ", [global, {return, list}]).


log_and_abort(Message) ->
	?CONSOLE("Aborting: ~s ~n",[Message]).


         
expand_sh_flag(return_on_error) ->
    {error_handler,
     fun(_Command, Err) ->
             {error, Err}
     end};
expand_sh_flag({abort_on_error, Message}) ->
    {error_handler,
		fun(Cmd,Err) ->
				log_and_abort({Message,{Cmd,Err}})
		end};
expand_sh_flag(abort_on_error) ->
	expand_sh_flag({abort_on_error, error});
expand_sh_flag(use_stdout) ->
    {output_handler,
     fun(Line, Acc) ->
             ?CONSOLE("~s", [Line]),
             [Line | Acc]
     end};
expand_sh_flag({use_stdout, false}) ->
    {output_handler,
     fun(Line, Acc) ->
             [Line | Acc]
     end};
expand_sh_flag({cd, _CdArg} = Cd) ->
    {port_settings, Cd};
expand_sh_flag({env, _EnvArg} = Env) ->
    {port_settings, Env}.


