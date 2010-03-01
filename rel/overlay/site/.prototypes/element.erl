-module (element_[[[NAME]]]).
-compile(export_all).
-include_lib("nitrogen/include/wf.hrl").

%% Move the following line to an include file:
-record([[[NAME]]], {?ELEMENT_BASE(element_[[[NAME]]])}).

reflect() -> record_info(fields, [[[NAME]]]).

render_element(_Record = #[[[NAME]]]{}) ->
    "<b>Hello from [[[NAME]]]</b>".
