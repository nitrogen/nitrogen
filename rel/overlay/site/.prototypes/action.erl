-module (action_[[[NAME]]]).
-compile (export_all).
-include_lib ("nitrogen/include/wf.hrl").

%% Move the following line to an include file:
-record([[[NAME]]], {?ACTION_BASE(action_[[[NAME]]])}).

render_action(_Record = #[[[NAME]]]{}) ->
    "alert('Hello, from [[[NAME]]]!');".
