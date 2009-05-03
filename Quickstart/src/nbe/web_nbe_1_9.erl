-module (web_nbe_1_9).
-include ("wf.inc").
-compile(export_all).

main() -> #template { file="./wwwroot/nbe.html" }.	

body() -> 
	[
		#button { text="Top", postback=top },
		#button { text="Bottom", postback=bottom },
		#button { text="Clear", postback=clear },
		#panel { style="border: solid 1px #bbb; margin: 12px; padding: 7px;", id=mypanel }
	].

event(top) -> 
	wf:insert_top(mypanel, #p { body="You clicked Top!" });
	
event(bottom) ->
	wf:insert_bottom(mypanel, #p { body="You clicked Bottom!" });
	
event(clear) ->
	wf:update(mypanel, []).
