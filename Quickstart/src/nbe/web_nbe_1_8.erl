-module (web_nbe_1_8).
-include ("wf.inc").
-compile(export_all).

main() -> #template { file="./wwwroot/nbe.html" }.	

body() -> [
	#button { id=mybutton, text="Button", postback=fill },
	#p{},
	#panel { id=mypanel }
].

event(fill) -> 
	Elements = [
		#span { text="You clicked the button!" },
		#br {},
		#link { text="Clear", postback=clear }
	],
	wf:update(mypanel, Elements);
	
event(clear) ->
	wf:update(mypanel, []).