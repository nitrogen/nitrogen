-module(nitrogen_skel).
-export([create_project/3]).

-include_lib("kernel/include/file.hrl").

%% TODO: There is no error checking at the moment.

create_project(Name, Dir, Type) ->
    create_tree(Dir),
    copy_nitrogen_www( filename:join([Dir, "wwwroot", "nitrogen"]), Name ),
    copy_template_files(Name, Dir, Type),
    ok.

%% Internal Functions
src_dir(Added) ->
    ModuleDir = filename:dirname(code:which(?MODULE)),
    filename:join([ModuleDir, "../", Added]).

create_tree(Dir) ->
    file:make_dir(Dir),
    Tree = ["/src", "/src/pages", "/ebin", "/doc", "/wwwroot", "/wwwroot/nitrogen", "/wwwroot/css", "/wwwroot/images"],
    [ file:make_dir(Dir ++ X) || X <- Tree].

copy_nitrogen_www(DestDir, Name) ->
    SrcDir = src_dir("www"),
    {ok, FileList} = file:list_dir(SrcDir),
    [ copy_file(src_dir("www/"++X), filename:join(DestDir, X), Name) || X <- FileList ].

copy_template_files(Name, DestDir, _Type) ->
    %% TODO: Start Managing Types
    FileList = [
                { src_dir("priv/skel/makefile"), filename:join(DestDir, "Makefile")},
                { src_dir("priv/skel/Emakefile"), filename:join(DestDir, "Emakefile")},
                { src_dir("priv/skel/wf_global.erl"), filename:join(DestDir, "src/wf_global.erl")},
                { src_dir("priv/skel/sync_configuration.erl"), filename:join(DestDir, "src/sync_configuration.erl")},
                { src_dir("priv/skel/web_index.erl"), filename:join(DestDir, "src/pages/web_index.erl")},
                { src_dir("priv/skel/template.html"), filename:join(DestDir, "wwwroot/template.html")},
                { src_dir("priv/skel/start.sh"), filename:join(DestDir, "start.sh")},
                %%{ src_dir("priv/skel/start-dev.bat"), filename:join(DestDir, "start-dev.bat")},
                
                { src_dir("priv/skel/SKEL.app"), filename:join(DestDir, "ebin/SKEL.app")},
                { src_dir("priv/skel/SKEL_app.erl"), filename:join(DestDir, "src/SKEL_app.erl")},
                { src_dir("priv/skel/SKEL_sup.erl"), filename:join(DestDir, "src/SKEL_sup.erl")}
               ],
    [ copy_file(X, Y, Name) || {X,Y} <- FileList ].


copy_file(Src, DestPath, Name) ->
    {ok, Dest, _} = regexp:gsub(DestPath, "SKEL", Name),
    io:format("writing -> ~p~n", [Dest]),
    {ok, Mode} = file:read_file_info(Src),
    {ok, Bin} = file:read_file(Src),
    Changes =  [{"SKEL", Name}],
    Replaced = nitrogen_project:replace_content(Changes, binary_to_list(Bin)),
    ok = file:write_file(Dest, list_to_binary(Replaced)),
    file:write_file_info(Dest, Mode).

