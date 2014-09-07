#!/usr/bin/env escript
%% -*- erlang -*-
%% vim: ts=4 sw=4 et ft=erlang
%%
%% Coyright 2013 - Jesse Gumm
%%
%% MIT License
%%
%% About
%%
%% This little script assists the Nitrogen Web Framework by simplifying the
%% process of including custom elements, actions, or validators without having
%% to worry about including a different header for each, or manually copying
%% records from each element's header into the "records.hrl".
%%
%% Further, this copies the plugin static information (javascript, images, etc)
%% into the site/static/plugins/<pluginname> directory. Then one doesn't need 
%% to copy that information for each upgrade.
%%
%% TO MAKE A PLUGIN WORK WITH THIS SCRIPT:
%%
%% 1) Make sure your plugin is able to be used as a rebar dependency
%% 2) Include a file "nitrogen.plugin" in the root of the plugin directory
%% 3) If you have any static resources (js, images) put them in a 'static'
%%    directory
%% 4) If this is a custom element, action, or validator, or requires anything
%%    else that should be included in a header, add them to (one or more) .hrl
%%    files in 'include'
%% 5) From your Nitrogen app's directory, include your plugin as a rebar
%%    dependency and run 'make'
%%
%% Sample Plugin Directory Structure:
%%
%% myplugin/
%%      ebin/
%%    
%%      src/
%%          element_myplugin.erl
%%          myplugin.app.src
%%      
%%      include/
%%          myplugin.hrl
%%      
%%      priv/
%%          templates/
%%              mytemplate.html
%%
%%          static/
%%              js/
%%                  myplugin.js
%%                  jquery-some-plugin.js
%%              images/
%%                  some_image.png
%%              css/
%%                  myplugin.css
%%
%%
%% When the plugin is processed, the following will be updated in your Nitrogen App
%%
%% site/
%%      include/
%%          plugins.hrl (will include a reference to all .hrl files from your plugin)
%%      static/
%%          plugins/
%%              myplugin/                
%%                  js/
%%                      myplugin.js
%%                      jquery-some-plugin.js
%%                  images/
%%                      some_image.png
%%                  css/
%%                      myplugin.css
%%      templates/
%%          plugins/
%%              mytemplate.html
%%
%% (Note: The Erlang/Nitrogen Element code is not copied, it'll be loaded just
%% like any rebar dependency's code)

main([]) ->
    io:format("Checking for Nitrogen Plugins\n"),
    RebarConfig = get_config("rebar.config"),
    PluginConfig = get_config("plugins.config"),
    DepDirs = proplists:get_value(deps_dir, RebarConfig, ["lib"]),
    {Includes,Statics,Templates} = lists:foldl(fun(Dir, {Inc, Stat, Temp}) ->
                            {ok, FoundIncludes, FoundStatics, FoundTemplates} = get_plugins(Dir),
                            {FoundIncludes ++ Inc, FoundStatics ++ Stat, FoundTemplates ++ Temp}
                        end, {[],[],[]}, DepDirs),
    case {Includes, Statics, Templates} of
        {[],[],[]} -> 
            io:format("No Nitrogen Plugins Found~n");
        _ ->
            io:format("Generating aggregate plugin header (plugins.hrl)~n"),
            generate_plugin_include(PluginConfig, Includes),
            io:format("Generating plugin static directories~n"),
            generate_plugin_static(PluginConfig, Statics),
            io:format("Generating plugin template directories~n"),
            generate_plugin_templates(PluginConfig, Templates),
            io:format("Plugin generation complete~n")
    end.


get_plugins(DepDir) ->
    Files = case file:list_dir(DepDir) of
        {ok, F} -> F;
        {error, _} -> []
    end,
    {Inc,Stat,Temp} = lists:foldl(fun(X,PluginInfo={Includes,Statics,Templates}) ->
                    PluginPath = filename:join(DepDir,X),
                    case analyze_path(PluginPath) of
                        undefined ->
                            %% Not a plugin, so just continue
                            PluginInfo;
                        {ok, FoundIncludes, FoundStatics, FoundTemplates} ->
                            {FoundIncludes++Includes, FoundStatics++Statics, FoundTemplates++Templates}
                    end
                end,{[],[],[]},Files),
    {ok, Inc, Stat, Temp}.
                          
get_config(File) ->
    case file:consult(File) of
        {error, _} -> [];
        {ok, Config} -> Config
    end.

analyze_path(Path) ->
    case is_dir_or_symlink_dir(Path) of
        true ->
            {ok, Files} = file:list_dir(Path),
            case lists:member("nitrogen.plugin",Files) of
                false -> 
                    undefined;
                true -> 
                    io:format("Found a Nitrogen plugin in ~p~n",[Path]),
                    IncludeDir = filename:join(Path,include),
                    StaticDir = filename:join(Path,static),
                    PrivStaticDir = filename:join([Path,priv,static]),
                    TemplateDir = filename:join([Path,priv,templates]),

                    Includes = analyze_path_include(IncludeDir),
                    %% Originally, the plugin spec called for statics to be
                    %% located in the "static" dir, however, it's more
                    %% OTP-compliant to have statics to be located in
                    %% "priv/static", so we support both here with StaticDir
                    %% and PrivStaticDir
                    Statics = analyze_path_exists_only(StaticDir) 
                              ++ analyze_path_exists_only(PrivStaticDir),

                    Templates = analyze_path_exists_only(TemplateDir),
                    {ok, Includes, Statics, Templates}
            end;
        false -> undefined
    end.

is_dir_or_symlink_dir(Path) ->
    case filelib:is_dir(Path) of
        true -> true;
        false ->
            case file:read_link(Path) of
                {ok, _} -> true;
                {error, _} -> false
            end
    end.
            

analyze_path_include(Path) ->
    case filelib:is_dir(Path) of
        false -> [];
        true -> list_includes(Path)
    end.
            
list_includes(Path) -> 
    {ok, Files} = file:list_dir(Path),
    Includes = filter_non_includes(Files),
    [filename:join(Path,Inc) || Inc <- Includes].

filter_non_includes([]) -> [];
filter_non_includes([File | Files]) ->
    case re:run(File,"\.hrl$",[{capture,none}]) of
        match -> [File | filter_non_includes(Files)];
        nomatch -> filter_non_includes(Files)
    end.

analyze_path_exists_only(Path) ->
    case filelib:is_dir(Path) of
        false -> [];
        true -> [Path]
    end.

generate_plugin_include(Config, Includes) ->
    IncludeFile = proplists:get_value(plugins_hrl, Config, "site/include/plugins.hrl"),
    HeaderLines = ["%% Automatically Generated by do-plugins.escript",
                   "%% Manually editing this file is not recommended."],
    PluginLines = [includify(I) || I <- Includes],
    PluginContents = string:join(HeaderLines ++ PluginLines,"\n"),
    FinalContents = iolist_to_binary(PluginContents),
    CurrentContents = get_current_include(IncludeFile),
    case FinalContents == CurrentContents of
        true -> io:format("No changes to ~p~n",[IncludeFile]);
        false -> file:write_file(IncludeFile,PluginContents)
    end.

get_current_include(File) ->
    case file:read_file(File) of
        {ok, Binary} -> Binary;
        {error, _} -> <<>>
    end.

includify("lib/" ++ Path) ->
    "-include_lib(\"" ++ Path ++ "\").";
includify(Path) ->
    "-include(\"" ++ filename:join("..",Path) ++ "\").".

generate_plugin_static(Config, Statics) ->
    PluginStaticBase = proplists:get_value(static_dir, Config, "site/static/plugins"),
    CopyMode = proplists:get_value(copy_mode, Config, copy),
    clear_plugin_dir(PluginStaticBase),
    filelib:ensure_dir(filename:join(PluginStaticBase,dummy)),
    [generate_plugin_copy_worker(PluginStaticBase,CopyMode,Static) || Static <- Statics].

clear_plugin_dir(Dir) ->
    rm_rf(Dir).

plugin_name_from_static_path(PluginStatic) ->
    PluginStaticParts = filename:split(PluginStatic),
    case lists:reverse(PluginStaticParts) of
        ["templates","priv",PluginName|_]   -> PluginName;
        ["templates",PluginName|_]          -> PluginName;
        ["static","priv",PluginName|_]      -> PluginName;
        ["static",PluginName|_]             -> PluginName
    end.

generate_plugin_copy_worker(PluginBase, CopyMode, PluginStatic) ->
    %% Split the Plugin Static Dir into parts and extract the name of the plugin
    %% ie "lib/whatever/static" - the Plugin Name is "whatever"
    PluginName = plugin_name_from_static_path(PluginStatic),
   
    %% And we're going to copy it into our system's plugin static dir
    %% (ie "site/static/plugins/whatever")
    FinalDestination = filename:join(PluginBase,PluginName),

    %% And let's copy or link them.
    case CopyMode of 
        link ->
            LinkPrefix = make_link_relative_prefix(FinalDestination),
               
            file:make_symlink(filename:join([LinkPrefix,PluginStatic]),FinalDestination);
        copy -> 
            %% We want to copy the contents of the Plugin's static dir,
            %% So we're copying from /lib/whatever/static/*
            PluginSource = filename:join(PluginStatic,"*"),

            %% Make sure the dir exists to copy into
            filelib:ensure_dir(filename:join(FinalDestination,dummy)),

            %% And copy the directory
            try cp_r([PluginSource],FinalDestination)
            catch _:_ -> ok  %% getting here usually just means that the source directory is empty
            end;
        Other ->
            throw({invalid_copy_mode, Other})
    end.

generate_plugin_templates(Config, Templates) ->
    TemplateBase = proplists:get_value(template_dir, Config, "site/templates/plugins"),
    CopyMode = proplists:get_value(copy_mode, Config, copy),
    clear_plugin_dir(TemplateBase),
    filelib:ensure_dir(filename:join(TemplateBase,dummy)),
    [generate_plugin_copy_worker(TemplateBase, CopyMode, Template) || Template <- Templates].

%% Because the symlink is relative, we need to make sure it includes the
%% proper relative path prefix (ie, the right number of "../../../" to get us
%% back before linking
make_link_relative_prefix("./" ++ Path) ->
    make_link_relative_prefix(Path);
make_link_relative_prefix(Path) ->
    Parts = filename:split(Path),
    Parts2 = lists:sublist(Parts, length(Parts)-1),
    Parts3 = [relative_path_helper(Part) || Part <- Parts2],
    filename:join(Parts3).

relative_path_helper(".") -> ".";
relative_path_helper("..") -> "";
relative_path_helper(_) -> "..".

%% -------------------------------------------------------------------------
%% ------------- File Actions (copied mostly from Rebar) -------------------
%% -------------------------------------------------------------------------


-define(CONSOLE(Str, Args), io:format(Str,Args)).
-define(FMT(Str,Args), lists:flatten(io_lib:format(Str,Args))).

rm_rf(Target) ->
    case os:type() of
        {unix, _} ->
            EscTarget = escape_spaces(Target),
            {ok, []} = sh(?FMT("rm -rf ~s", [EscTarget]),
                                      [{use_stdout, false}, return_on_error]),
            ok;
        {win32, _} ->
            Filelist = filelib:wildcard(Target),
            Dirs = [F || F <- Filelist, filelib:is_dir(F)],
            Files = Filelist -- Dirs,
            ok = delete_each(Files),
            ok = delete_each_dir_win32(Dirs),
            ok
    end.

delete_each([]) ->
    ok;
delete_each([File | Rest]) ->
    case file:delete(File) of
        ok ->
            delete_each(Rest);
        {error, enoent} ->
            delete_each(Rest);
        {error, Reason} ->
            ExitMsg = ?FMT("Error: Failed to delete file ~s: ~p\n", [File, Reason]),
            exit(ExitMsg)
    end.

delete_each_dir_win32([]) -> ok;
delete_each_dir_win32([Dir | Rest]) ->
    {ok, []} = sh(?FMT("rd /q /s \"~s\"",
                                   [filename:nativename(Dir)]),
                              [{use_stdout, false}, return_on_error]),
    delete_each_dir_win32(Rest).



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

%%get_cwd() ->
%%	{ok,Dir} = file:get_cwd(),
%%	Dir.

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


