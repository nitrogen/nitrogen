-module(nitrogen_helpers).
-include_lib("nitrogen_core/include/wf.hrl").
-export([build_dispatch/0, template/1]).

%% Given a template name, return a reasonable file path.
template(Name) ->
  filename:join([code:priv_dir([-$PROJECT-]), templates, Name]).

build_dispatch() ->
  {_Root, Paths} = simple_bridge_util:get_docroot_and_static_paths(cowboy),
  build_dispatch(Paths).

build_dispatch(StaticPaths) ->
  StaticDispatches = lists:map(fun(Dir) ->
    Opts = [{mimetypes, cow_mimetypes, all}],
    Path = reformat_path(Dir),
    {Type, RelPath} = localized_dir_file(Dir),
    {Path, cowboy_static, {Type, [-$PROJECT-], RelPath, Opts}}
  end, StaticPaths),
  HandlerModule = simple_bridge_util:get_anchor_module(cowboy),
  HandlerOpts = [],
  Dispatch = [{'_', StaticDispatches ++ [{'_', HandlerModule , HandlerOpts}]}],
  cowboy_router:compile(Dispatch).

reformat_path(Path) ->
  Path2 = case hd(Path) of
    $/ -> Path;
    $\ -> Path;
    _ -> [$/|Path]
  end,
  Path3 = case lists:last(Path) of 
    $/ -> Path2 ++ "[...]";
    $\ -> Path2 ++ "[...]";
    _ -> Path2
  end,
  Path3.

localized_dir_file(Path) ->
  NewPath = case hd(Path) of
    $/ -> "static" ++ Path;
    _ ->  "static" ++ "/" ++ Path
  end,
  _NewPath2 = case lists:last(Path) of
    $/ -> {priv_dir, NewPath};
    _ ->  {priv_file, NewPath}
  end.
