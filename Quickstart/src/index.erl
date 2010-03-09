-module (index).
-include ("wf.inc").
-compile(export_all).

main() -> #template { file="./templates/front.html", bindings=[
	{'Group', home},
	{'Item', none}
]}.

title() -> "Nitrogen Web Framework for Erlang".
headline() -> "".

body() -> [
	"
	<div style='margin-left: 22%'>
	<a href='/web/learn' border=0>
	<img src=/images/giantleap.png>
	</a>
	</div>
	"
].

side() -> [].
	
event(_) -> ok.
