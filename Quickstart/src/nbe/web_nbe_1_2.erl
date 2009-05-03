-module (web_nbe_1_2).
-include ("wf.inc").
-compile(export_all).

main() -> #template { file="./wwwroot/nbe.html" }.	

body() -> [
	#h1 { text="Some sample elements..." },
	#label { text="Label" },
	#p{},
	#hr {},
	#p{},
	#button { text="Button" },
	#panel { body=[
		#span { text="Nested controls" },
		#p{},
		#textbox { text="Textbox" },
		#p{},
		#checkbox { text="Checkbox" }		
	]}
].