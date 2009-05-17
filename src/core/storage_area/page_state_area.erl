-module (page_state_area).
-behaviour (nitrogen_storage_area).
-compile(export_all).
-include ("simplebridge.hrl").

init(Name, PageModule, RequestBridge) -> page_state_area.

get(Key, DefaultValue) -> DefaultValue.

put(Key, Value) -> page_state_area.

clear(Key) -> page_state_area.

clear_all() -> page_state_area.

to_js() -> "".
