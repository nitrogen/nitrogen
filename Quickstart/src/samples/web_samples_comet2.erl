-module (web_samples_comet2).
-include ("wf.inc").
-compile(export_all).

main() -> #template { file="./wwwroot/onecolumn.html", bindings=[
	{'Group', learn},
	{'Item', samples}
]}.

title() -> "Comet Chatroom".
headline() -> "Comet Chatroom".
right() -> linecount:render().

body() ->
	ensure_chatroom_running(),
	
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
	Pid = wf:comet(fun() -> listen_for_messages() end),
	chatroom!{join, Pid},

	wf:render(Body).
	

event(chat) ->
	[UserName] = wf:q(userNameTextBox),
	[Message] = wf:q(messageTextBox),
	chatroom!{message, UserName, Message},
	wf:wire("obj('messageTextBox').focus(); obj('messageTextBox').select();");
	
event(_) -> ok.

listen_for_messages() ->
	receive	{message, UserName, Message} -> 	
		Terms = [
			#p{},
			#span { text=UserName, class=username }, ": ",
			#span { text=Message, class=message }
		],
		wf:insert_bottom(chatHistory, Terms),
		wf:wire("obj('chatHistory').scrollTop = obj('chatHistory').scrollHeight;"),
		wf:comet_flush()	
	end,
	listen_for_messages().

%%% CHATROOM %%%

ensure_chatroom_running() ->
	ChatPid = whereis(chatroom),
	IsChatAlive = ChatPid /= undefined andalso is_process_alive(ChatPid),
	case IsChatAlive of 
		true -> ok;
		false -> 
			NewChatPid = spawn(fun() -> chatroom_loop([]) end),
			register(chatroom, NewChatPid)
	end.
	
chatroom_loop(Users) ->
	receive
		{join, UserPid} -> 
			erlang:monitor(process, UserPid),
			chatroom_loop([UserPid|Users]);
			
		{'DOWN', _MonitorRef, process, UserPid, _Info} ->
			chatroom_loop(Users -- [UserPid]);
			
		{message, UserName, Message} ->
			[X!{message, UserName, Message} || X <- Users],
			chatroom_loop(Users)
	end.
	
