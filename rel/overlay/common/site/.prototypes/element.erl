%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_[[[NAME]]]).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-export([
    reflect/0,
    render_element/1
]).

%% Move the following record definition to records.hrl:
-record([[[NAME]]], {?ELEMENT_BASE(element_[[[NAME]]]),
        attr1 :: any(),
        attr2 :: any()
    }).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, [[[NAME]]]).

-spec render_element(#[[[NAME]]]{}) -> body().
render_element(_Record = #[[[NAME]]]{}) ->
    "<b>Hello from [[[NAME]]]</b>".
