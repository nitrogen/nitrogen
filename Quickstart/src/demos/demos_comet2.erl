-module (demos_comet2).
-include_lib ("nitrogen/include/wf.hrl").
-compile(export_all).

main() -> #template { file="./templates/demos46.html", bindings=[
	{'Group', learn},
	{'Item', samples}
]}.

title() -> "Comet Chatroom".
headline() -> "Comet Chatroom".
right() -> linecount:render().

body() ->
	Body=[
		#p{},
		"
			This page uses comet to create a chatroom. Open this page in different browser
			windows and try chatting to yourself.
		",
		
		#p{},
		#span { text="Your chatroom name: " }, 
		#textbox { id=userNameTextBox, text="Anonymous", style="width: 100px;", next=messageTextBox },
		
		#p{},
		#panel { id=chatHistory, class=chat_history },
		
		#p{},
		#textbox { id=messageTextBox, style="width: 70%;", next=sendButton },
		#button { id=sendButton, text="Send", postback=chat }
	],
	
	% Start a process to listen for messages,
	% and then tell the chatroom that we would like to join.
	wf:comet_global(fun() -> chat_loop() end, chatroom),

	Body.
	

event(chat) ->
	Username = wf:q(userNameTextBox),
	Message = wf:q(messageTextBox),
	wf:send_global(chatroom, {message, Username, Message}),
	wf:wire("obj('messageTextBox').focus(); obj('messageTextBox').select();");
	
event(_) -> ok.

chat_loop() ->
	receive {message, Username, Message} ->
		Terms = [
			#p{},
			#span { text=Username, class=username }, ": ",
			#span { text=Message, class=message }
		],
		wf:insert_bottom(chatHistory, Terms),
		wf:wire("obj('chatHistory').scrollTop = obj('chatHistory').scrollHeight;"),
		wf:flush()	
	end,
	chat_loop().	
