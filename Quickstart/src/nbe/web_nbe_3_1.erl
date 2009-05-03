% ./src/nbe/web_nbe_3_1.erl
-module (web_nbe_3_1).
-include ("wf.inc").
-include ("click_for_message.inc").
-compile(export_all).

main() -> #template { file="./wwwroot/nbe.html" }.	

body() -> 
	[
		#click_for_message { button_text="Button", message_text="You clicked the button!" }
	].
