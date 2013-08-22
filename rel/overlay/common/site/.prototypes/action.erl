%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (action_[[[NAME]]]).
-include_lib ("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-export([
    render_action/1
]).

%% move the following record definition to site/include/records.hrl
-record([[[NAME]]], {?ACTION_BASE(action_[[[NAME]]]),
        attr1 :: any(),
        attr2 :: any()
    }).

-spec render_action(#[[[NAME]]]{}) -> actions().
render_action(_Record = #[[[NAME]]]{}) ->
    "alert('Hello, from [[[NAME]]]!');".
