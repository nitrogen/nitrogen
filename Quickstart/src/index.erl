-module (index).
-include ("wf.inc").
-compile(export_all).

main() -> #template { file="./templates/front.html", bindings=[
	{'Group', home},
	{'Item', none}
]}.

title() -> "Nitrogen Web Framework for Erlang".
