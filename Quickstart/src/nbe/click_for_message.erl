% ./src/nbe/click_for_message.erl
-module (click_for_message).
-include ("wf.inc").
-include ("click_for_message.inc").
-compile(export_all).

reflect() -> record_info(fields, click_for_message).

%%% CODE %%%

render(_ControlID, Record) -> 
	SpanID = wf:temp_id(),
	[
		#button { text=Record#click_for_message.button_text, actions=[
			#event { 
				delegate=?MODULE,
				type=click, 
				postback={click, SpanID, Record#click_for_message.message_text} 
			}
		]},
		#span { id=SpanID }
	].

event({click, SpanID, MessageText}) ->
	wf:update(SpanID, MessageText).