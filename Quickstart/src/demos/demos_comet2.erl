-module (demos_comet2).
-include_lib ("nitrogen/include/wf.hrl").
-compile(export_all).

main() -> #template { file="./templates/demos46.html" }.

title() -> "Comet Chatroom".

headline() -> "Comet Chatroom".

left() -> 
    [
        "
        <p>
        This page demonstrates how to make a simple chatroom using
        Comet pools. A Comet pool provides the plumbing for creating
        multi-user applications in Nitrogen.

        <p>
        Each comet pool has an Erlang term identifier, and can either
        be <i>local</i>, meaning that it only applies to one user, or
        <i>global</i> meaning that it applies to all users.

        <p>
        Here, we create a global Comet pool titled 'chatroom'. Anyone
        who visits this page connects to the same comet pool.  Upon
        clicking the 'Send' button, a message is broadcast to all
        comet processes that have registered in the pool. The comet process
        then updates the page with the new message.
        
        <p>
        Try opening a few different browser windows and chatting with
        yourself.
        ",
        linecount:render()
    ].

right() ->
    Body=[
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
    receive 
        'INIT' ->
            %% The init message is sent to the first process in a comet pool.
            Terms = [
                #p{},
                #span { text="You are the only person in the chat room.", class=message }
            ],
            wf:insert_bottom(chatHistory, Terms),
            wf:flush();

        {message, Username, Message} ->
            %% We got a message, so show it!
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
