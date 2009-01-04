-module(nitrogen_skel).
-export([create_project/3]).

-include_lib("kernel/include/file.hrl").

%% TODO: There is no error checking at the moment.

create_project(Name, Dir, Type) ->
    create_tree(Dir),
    copy_nitrogen_www( filename:join([Dir, "wwwroot", "nitrogen"]) ),
    copy_template_files(Dir, Type),
    ok.

%% Internal Functions
src_dir(Added) ->
    ModuleDir = filename:dirname(code:which(?MODULE)),
    filename:join([ModuleDir, "../", Added]).

create_tree(Dir) ->
    file:make_dir(Dir),
    Tree = ["/src", "/src/pages", "/ebin", "/doc", "/wwwroot", "/wwwroot/nitrogen", "/wwwroot/css", "/wwwroot/images"],
    [ file:make_dir(Dir ++ X) || X <- Tree].

copy_nitrogen_www(DestDir) ->
    SrcDir = src_dir("www"),
    {ok, FileList} = file:list_dir(SrcDir),
    [ copy_file(X, X, SrcDir, DestDir) || X <- FileList ].

copy_template_files(DestDir, _Type) ->
    %% TODO: Start Managing Types
    FileList = [
                {"Makefile", "Makefile"},
                {"Emakefile", "Emakefile"},

                {"wf_global.erl", "src/wf_global.erl"},
                {"sync_configuration.erl", "src/sync_configuration.erl"},
                {"web_index.erl", "src/pages/web_index.erl"},
                {"template.html", "wwwroot/template.html"},

                {"start-dev.sh", "start-dev.sh"},
                {"start-dev.bat","start-dev.bat"}
               ],
    [ copy_file(X, Y, src_dir("priv/skel"), DestDir) || {X,Y} <- FileList ].


copy_file(SrcFile, DestFile, SrcPath, DestPath) ->
    Src = filename:join(SrcPath, SrcFile),
    Dest = filename:join(DestPath, DestFile),
    {ok, Mode} = file:read_file_info(Src),
    file:copy(Src, Dest),
    file:write_file_info(Dest, Mode).

