-module (footer).
-include_lib ("nitrogen/include/wf.hrl").
-compile(export_all).

render() -> 
    #panel { class=content, body=[
        "
        Nitrogen Web Framework.
        "
    ]}.
