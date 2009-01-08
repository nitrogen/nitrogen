% Nitrogen Web Framework for Erlang
% Copyright (c) 2008 Rusty Klophaus
% Module by Jon Gretar Borgthorsson
% See MIT-LICENSE for licensing information.
-module(nitrogen_project).
-include_lib("kernel/include/file.hrl").
-export([create_project/2, src_path/1]).

%% TODO: There is no error checking at the moment.

create_project(Name, Dir) ->
    create_tree(Dir),
    copy_nitrogen_www( filename:join([Dir, "wwwroot", "nitrogen"])),
    copy_template_files(Name, Dir),
    ok.

src_path(Path) ->
    ModuleDir = filename:dirname(code:which(?MODULE)),
    filename:join([ModuleDir, "../", Path]).

%% Internal Functions

create_tree(Dir) ->
    file:make_dir(Dir),
    Tree = ["src", "src/pages", "ebin", "doc", "wwwroot", "wwwroot/nitrogen", "wwwroot/css", "wwwroot/images"],
    [ file:make_dir(filename:join(Dir,X)) || X <- Tree].

copy_nitrogen_www(DestDir) ->
    SrcDir = src_path("www"),
    {ok, FileList} = file:list_dir(SrcDir),
    [ nitrogen_file:copy_file(src_path("www/"++X), filename:join(DestDir, X)) || X <- FileList ].

copy_template_files(Name, DestDir) ->
    Changes = [{"PROJECT", Name},{"PAGE", "web_index"}],
    FileList = [
                { "priv/skel/Makefile", filename:join(DestDir, "Makefile")},
                { "priv/skel/Emakefile", filename:join(DestDir, "Emakefile")},
                { "priv/skel/template.html", filename:join(DestDir, "wwwroot/template.html")},
                { "priv/skel/start.sh", filename:join(DestDir, "start.sh")},
                { "priv/skel/PAGE.erl", filename:join(DestDir, "src/pages/web_index.erl")},
                { "priv/skel/PROJECT.app", filename:join(DestDir, "ebin/PROJECT.app")},
                { "priv/skel/PROJECT_app.erl", filename:join(DestDir, "src/PROJECT_app.erl")}
               ],
    [ nitrogen_file:copy_file(src_path(Src), Dest, Changes) || {Src,Dest} <- FileList ].

