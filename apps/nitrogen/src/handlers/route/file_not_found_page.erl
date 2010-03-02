-module (file_not_found_page).
-include_lib ("wf.hrl").
-export ([main/0]).

main() ->
    PathInfo = wf_context:path_info(),
    wf:info("File not found: ~p", [PathInfo]),
    [
        "404 - Not found."
    ].
