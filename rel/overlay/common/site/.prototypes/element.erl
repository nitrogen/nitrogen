%% -*- mode: nitrogen -*-
-module (element_[[[NAME]]]).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

%% Move the following line to records.hrl:
-record([[[NAME]]], {?ELEMENT_BASE(element_[[[NAME]]]), attr1, attr2}).

reflect() -> record_info(fields, [[[NAME]]]).

render_element(_Record = #[[[NAME]]]{}) ->
    "<b>Hello from [[[NAME]]]</b>".
