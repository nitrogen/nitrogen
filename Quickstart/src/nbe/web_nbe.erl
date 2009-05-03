-module (web_nbe).
-include ("wf.inc").
-compile(export_all).

% Template bindings.

main() -> #template { file="./wwwroot/nbe.html" }.

body() -> "Index".


%%% FUNCTIONS USED BY ALL NBE EXAMPLES %%%

title() -> 
	PageModule = wf:get_page_module(),
	title(PageModule).

header() -> 
	PageModule = wf:get_page_module(),
	[
		#span { class=title, text=title() },
		#panel { class=boxes_outer, body=[
			#singlerow { class=boxes, cells=[make_box(X, PageModule) || X <- pages()]}
		]}
	].
	
footer() ->
	PageModule = wf:get_page_module(),
	PrevPage = get_previous(PageModule),
	NextPage = get_next(PageModule), 
	[
		#singlerow { class=footer, cells=[
			#tablecell { class=back, body=#link { show_if=(PrevPage /= undefined), text="<< " ++ title(PrevPage), url=to_url(PrevPage) } },
			#tablecell { class=viewsource, body=[
				#link { id=viewnotes, style="display: none;", text="View Notes", actions=[
					#event { type=click, actions=[
						#hide { target=viewnotes },
						#hide { target=source },
						#appear { target=viewsource },
						#appear { target=notes }
					]}
				]},
				#link { id=viewsource, text="View Source", actions=[
					#event { type=click, actions=[
						#hide { target=viewsource },
						#hide { target=notes },
						#appear { target=viewnotes },
						#appear { target=source }
					]}
				]}
			]},
			#tablecell { class=next, body=#link { show_if=(NextPage /= undefined), text=title(NextPage) ++ " >>", url=to_url(NextPage) } }
		]},
		
		#panel { id=source, style="position: absolute; display: none; float:left;", class=source, body=[
			get_source(PageModule)
		]},
		#panel { id=notes, style="float: left;", class=notes, body=[
			notes(PageModule)
		]}
	].
	
pages() -> [
	web_nbe_1_1, web_nbe_1_2, web_nbe_1_3, web_nbe_1_4, web_nbe_1_5, web_nbe_1_6, web_nbe_1_7, web_nbe_1_8, web_nbe_1_9,
	web_nbe_2_1, web_nbe_2_2, web_nbe_2_3, web_nbe_2_4, web_nbe_2_5,
	web_nbe_3_1, web_nbe_3_2, web_nbe_3_3,
	web_nbe_4_1, web_nbe_4_2, web_nbe_4_3,
	web_nbe_5_1, web_nbe_5_2, web_nbe_5_3,
	web_nbe_6_1, web_nbe_6_2, web_nbe_6_3
].

title(web_nbe) -> "Index";

title(web_nbe_1_1) -> "Hello World!";
title(web_nbe_1_2) -> "Elements";
title(web_nbe_1_3) -> "Postbacks";
title(web_nbe_1_4) -> "More Postbacks";
title(web_nbe_1_5) -> "Other Events";
title(web_nbe_1_6) -> "Using wf:wire";
title(web_nbe_1_7) -> "Complex Tags";
title(web_nbe_1_8) -> "Complex Updates";
title(web_nbe_1_9) -> "Insert Top, Insert Bottom";

title(web_nbe_2_1) -> "Actions";
title(web_nbe_2_2) -> "Effects";
title(web_nbe_2_3) -> "Avoiding Postbacks";
title(web_nbe_2_4) -> "Validation";
title(web_nbe_2_5) -> "#custom and #js_custom";

title(web_nbe_3_1) -> "Custom Elements";
title(web_nbe_3_2) -> "Custom Actions";
title(web_nbe_3_3) -> "Custom Validators";

title(web_nbe_4_1) -> "Sorting";
title(web_nbe_4_2) -> "Drag and Drop";
title(web_nbe_4_3) -> "Upload";

title(web_nbe_5_1) -> "Page State";
title(web_nbe_5_2) -> "Session State";
title(web_nbe_5_3) -> "Users and Roles";

title(web_nbe_6_1) -> "Comet";
title(web_nbe_6_2) -> "Continuations";
title(web_nbe_6_3) -> "Windex";

title(_) -> "Untitled".

notes(web_nbe_1_1) ->
	"
	Each Nitrogen page is a module that exposes a main/0 function. By default, 
	the name of the module determines the path of the page. The module for this 
	page is named 'web_nbe_1_1.erl', so it responds to '/web/nbe/1/1'.
	<p>
	To render a page, Nitrogen calls the main/0 function of the module. In most
	cases, the main/0 function returns a #template element and specifies a template
	file with callouts to other functions.
	<p>
	In this case, the template file contains a callout to the body/0 function of this module, as well 
	as other callouts to create the header and footer for this tutorial. A callout
	takes the form [[[Module:Function(Args)]]]. Specifying a module of 'page' tells 
	Nitrogen to use the current page module.
	<p>
	<a href='/web/nbe/template' target=_blank>Click here</a> to view the template file.
	";
	
notes(web_nbe_1_2) ->
	"
	In Nitrogen, HTML is constructed using elements, which are really just
	Erlang records with a backing module. Later on, we'll create a custom
	element which will give you more insight into how elements work.
	<p>
	For now, all you need to know is that Nitrogen comes with a large set of
	elements that cover every commonly used HTML element out there.
	<p>
	If you do need an element that Nitrogen doesn't include, you can either
	create your own custom element, or just drop down into raw HTML mode. Click
	the \"View Source\" link to see how it is done.
	";

notes(web_nbe_1_3) ->
	"
	Nitrogen lets you write server side code to manipulate client side elements.
	The transfer of control from the client to the server is called a postback.
	When a postback occurs, the client side state is bundled up and transfered to
	the server, along with information about the event that triggered the
	postback. (This mimics the ASP.NET model of postbacks and viewstate.)
	<p>
	On this page, when you click the button, the page triggers a postback,
	calling the event/1 function. The 'postback' attribute tells Nitrogen
	to make this happen.
	<p>
	event/1 calls wf:update/2 to change the contents of a span on the page.
	This update--like all postbacks and updates in Nitrogen--happens via Ajax.
	<p>
	The next few examples in the tutorial demonstrate different ways of wiring 
	postbacks, and how to pass information along with a postback.
	";

notes(web_nbe_1_4) ->
	"
	Two new topics are introduced in this example:
	<p>
	First, we show another method of wiring an event to a button. In the 
	previous example, you saw how to use the 'postback' attribute to listen 
	for a click event. Another way to tell Nitrogen to listen for a click 
	event by setting providing an #event action to the 'actions' attribute. 
	This is how Button 2 knows to listen for a click.
	<p>
	Second, we show how to differentiate between events. If you
	view the source code for this example you will see that the event/1 function
	runs one block of code when it receives 'button1', and a different block
	when it receives 'button2'.
	<p>
	Over the next few examples, you will see how to listen for a wide variety of
	events, and how to include more information with a postback.
	";
	
notes(web_nbe_1_5) ->
	"
	You can tell Nitrogen to respond to more than just mouse clicks. The
	example above shows how to listen for mouseover and mouseout events
	by passing an array of #events to the 'actions' attribute of the button.
	<p>
	Using the 'actions' attribute, you can tell any element in Nitrogen 
	to respond to events, and you can listen for any Javascript event. 
	(ie: blur, change, click, focus, mouseover, mouseout, mousedown, mouseup, etc.)
	<p>
	If you have a large number of elements and events, this syntax can quickly
	get clumsy, so on the next page, we show how to use the wf:wire/2 function
	to separate your events from your elements.
	";
	
notes(web_nbe_1_6) ->
	"
	This page contains the same functionality as the previous example, 
	except here the events are not specified inline with the elements. 
	Instead, this page uses the wf:wire/2 function.
	<p>
	wf:wire/2 works the same as the 'actions' attribute, the only drawback
	is that you have to name an element using the 'id' attribute before you can
	wire any actions to it.	
	";
	
notes(web_nbe_1_7) ->
	"
	One of Nitrogen's most powerful features is the ability to serialize and
	send back an arbitrary amount of data during a postback. In this example,
	Nitrogen tells the event/1 function not only which span to update, but also
	what message to use.
	<p>
	Most frameworks would require you to roll your own serialization using a 
	combination of hidden fields, form values, or session state to accomplish 
	the same thing.
	";
	
notes(web_nbe_1_8) ->
	"
	So far, we have only demonstrated how to use wf:update/2 with a simple
	text string. Nitrogen actually allows you to pass entire blocks of 
	Nitrogen elements to the wf:update/2 function.
	<p>
	In this example, when you click on the button, Nitrogen updates a #panel
	element (essentially a &lt;div&gt;) with some text and a link using the 
	#span and #link elements. When you click the link, the contents of
	the panel are cleared.
	<p>
	This illustrates a key component of Nitrogen: Consistency. You use the
	same style of coding to both create AND update the page. 
	";

notes(web_nbe_1_9) ->
	"
	wf:update/2 has two cousins, wf:insert_top/2 and wf_insert_bottom/2. These
	functions do the same thing as wf:update/2, except they leave the existing
	content in place and just add new elements at the top or bottom, respectively.
	";
	
notes(web_nbe_2_1) ->
	"
	By now, you should begin to see the distinction between the two main Nitrogen
	building blocks, elements and actions:
	<ul>
		<li><i>elements</i> are for showing things.
		<li><i>actions</i> are for making things happen.
	</ul>
	<p>
	You have already seen how actions can be used to listen for and react to
	events on the page. The next few examples show how actions can be used
	to run Ajax effects.	
	";
	
notes(web_nbe_2_2) ->
	"
	When most people think of Ajax, they think of Ajax effects. Nitrogen
	supports a full range of JQuery based effects which can be applied to 
	an element using the 'actions' attribute, or using wf:wire/2.
	<p>
	In this example, when you click on the button, Nitrogen applies a 'pulsate'
	effect to the text beside it.
	<p>
	The next example shows how to accomplish the same result without requiring
	a server side postback.
	";
	
notes(web_nbe_2_3) ->
	"
	The event tag doesn't always need to initiate a postback to the server; it
	can also be used to conditionally fire effects or other Javascript on
	the client.
	<p>
	This page contains the same functionality as the previous example, except
	the #effect action is wired directly to the #event action, avoiding a postback.
	<p>
	In order to tell the effect what to act on, we use the 'target' attribute. The 'target'
	attribute, along with the 'trigger' attribute, allow you to wire events and effects
	with precision:
	<ul>
		<li>'trigger' specifies which element fires the event.
	 	<li>'target' specifies which element receives the effect.
	</ul>
	";

notes(web_nbe_2_4) ->
	"
	We have already seen two types of actions: events and effects. A third
	type of action is the #validate action. The #validate action allows you
	to validate form elements during a postback.
	<p>
	Nitrogen is different from most validation you have seen elsewhere in that
	it runs validation on both the client AND the server. The pattern for writing
	validators allows you to specify Javascript validation code and Erlang validation 
	code, and all of the validators that ship with Nitrogen include both. (Client side
	validation uses the LiveValidation.js library.)
	<p>
	On this page, we validate to ensure that the textbox is not empty and that it 
	contains less than 10 characters. 
	<p>
	Validation is wired using wf:wire/3. This is very similar to wf:wire/2.
	<ul>
	<li>The first argument contains the trigger, or the element that will cause validation to occur.
	<li>The second argument contains the target, or the element that will be validated.
	<li>The third argument contains the #validate action, which in turn takes a list of validators.
	</ul>
	";
	
notes(web_nbe_2_5) ->
	"
	Nitrogen lets you create new validation logic in three ways:
	<ul>
	<li>Validate on the client with a Javascript function using the #js_custom validator
	<li>Validate on the server with an Erlang function using the #custom validator 
	<li>Write a reusable validator by creating a #myvalidator record and myvalidator.erl module
	</ul>
	<p>
	This example illustrates the first two ways of doing custom validation. Here,
	we use the #js_custom validator to check that the value starts with an 'A', and
	a #custom validator to check that the value ends with a 'Z'. Note that custom
	validators are only run if the field contains a value.
	<p>
	A later example shows how to turn this into a reusable validator.
	";
	
notes(web_nbe_3_1) ->
	"
	If you find yourself repeatedly using the same block of elements across your
	site, you can encapsulate and easily reuse that block by making a custom
	element.
	<p>
	In code, you refer to your custom element the same way you refer to any
	other element, ie: #myelement { attribute=value, ... }.
	<p>
	A custom element takes two parts, a record definition that defines what 
	attributes are used, and a backing module that contains functions to
	render the element and respond to any events.
	<p>
	This example uses a custom element called #click_for_message. It shows
	a button that displays a message when clicked. Notice the use of the 'delegate'
	attribute in the event, this tells Nitrogen to delegate the postback 
	to the 'click_for_message' module instead of the current page.
	";
	
notes(web_nbe_3_2) ->
	"
	A custom action is similar in structure to a custom element, except
	a custom action allows you to encapsulate Javascript rather than HTML.
	<p>
	The custom action on this page, #backwards_alert, uses the Javascript
	alert() function to display a string of text backwards.
	";
	
notes(web_nbe_3_3) ->
	"
	A custom action is similar in structure to a custom element or action,
	except a custom validator allows you to encapsulate validation logic.
	<p>
	The custom validator on this page, #is_a_z, insists that a value start with the letter
	'A' and end with the letter 'Z'.
	";
	

notes(web_nbe_4_1) ->
	"
	So far, you have seen the rudimentary building blocks of Nitrogen;
	basic elements, events, postbacks, and validation.
	<p>
	The next few examples demonstrate advanced features of Nitrogen. Nitrogen
	cleanly encapsulates what's happening behind the scenes and allows you to 
	capture events in a consistent manner.	
	<p>
	In this example, the items are sortable. Each item can be assigned a 
	unique 'tag' that can be any Erlang term. After a sort event, Nitrogen calls 
	the sort_event/2 function, passing in a list of tags in their new order.
	<p>
	Notice the close consistency to the event/1 method of handling postbacks.
	";
	
notes(web_nbe_4_2) ->
	"
	This example demonstrates how to utilize Nitrogen's drag-and-drop functionality. Again,
	each item is given a unique tag, and when a drop event occurs,
	Nitrogen calls the drop_event/2 function, passing in the drag tag
	and the drop tag associated with the dragged item and the drop target,
	respectively.
	";
	
notes(web_nbe_4_3) ->
	"
	File uploads are a commonly used&mdash;and commonly painful&mdash;experience in creating 
	websites. Not so in Nitrogen.
	<p>
	Nitrogen applies the same consistent model of event handling to uploads. This
	example shows how the #upload tag is placed on a page. When the user selects
	a file and clicks the upload button, the file is sent to the server and stored
	in a temporary file.
	<p>
	Nitrogen then calls an upload_event/3 method, passing in the name of the uploaded
	file, and the full path to the temporary file, allowing your program to access the
	files contents.
	";
	
notes(web_nbe_5_1) ->
	"
	We will now switch modes to discuss some of the less visual aspects of Nitrogen
	development, starting with where to store values.
	<p>
	Nitrogen offers two different key/value storage areas for you to use, depending
	on your needs. in both types of storage, the keys and values can be any Erlang term.
	<p>
	The first storage area we will examine is Page State. Page State is read using
	the wf:state/1 function, and modified using the wf:state/2 function. The data is 
	encoded and digitally signed so that you can trust the values it contains. 
	(Note, it is not encrypted, so it is not a good place to store sensitive data.)
	<p>
	The important thing to note is that page state \"lives\" in the browser as a Javascript
	variable, which gives it some important properties:
	<ul>
		<li>It uses no server resources, reducing load on the server.
		<li>It is not tied to any one server, allowing you to scale out to many nodes without
				worrying about requests going back to the right node.
		<li>It lives only as long as the current page, and disappears as soon as
				the user navigates to a new page.
	</ul>
	This example uses Page State to create a simple counter. 
	";

notes(web_nbe_5_2) ->
	"
	The second storage area that Nitrogen offers is Session State.
	<p>
	Session State is read using the wf:session/1 function, and modified using the 
	wf:session/2 function. 
	
	The data lives on the server, which gives it some important properties:
	<ul>
		<li>It takes up server memory, so should be used sparingly.
		<li>It lives within a gen_server process, so is accessible across
				any node on the cluster, allowing you to scale out to many nodes
				without worrying about requests going to the right node.
		<li>It lasts for a configurable amount of time after the user stops browsing,
				20 minutes by default.
	</ul>
	
	This example uses Session State to create a simple counter. Open
	a new window and increment the counter in each window to see how
	Session State is shared.
	";
	
notes(web_nbe_5_3) ->
	"
	Nitrogen provides functions for working with users and roles. It is
	important to note that Nitrogen does not store any user or role information
	to disk, instead this is all kept in the users session. The user and role
	functions are just a thin wrapper of convenience on top of wf:session.
	<p>
	The wf:user/1 function can be used to set the current user, and the 
	wf:user/0 function can be used to retrieve the current user.
	<p>
	Likewise, the wf:role/2 function can be used to set whether a user is in
	a role, and the wf:role/1 function can be used to retrieve whether
	a user is in a role.
	<p>
	This example shows a simple login screen. Enter any username and password,
	click login, and then open this page in a new browser window to this page 
	to see how your authentication carries across your entire session.	
	";


% notes(web_nbe_6_1) ->
% 	"
% 	Comet, also called long polling, is a version of Ajax where the browser
% 	issues a request for information from the server and then waits until that
% 	information is available. For example, Web-based chat typically uses Comet
% 	in order to allow new chat messages as soon as they are available, rather
% 	than requiring the browser to continuously spawn periodic Ajax calls.
% 	<p>
% 	Nitrogen lets you use Comet as easily as spawning a new process via the wf:comet/1
% 	function. This example uses Comet to create a simple counter that updates once per second
% 	and automatically terminates shortly after you leave the page.
% 	<p>
% 	The main advantage of Comet is a snappier interface, but the main disadvantage
% 	is that it hogs an available thread on the client and an available connection
% 	on the server. For that reason, Comet should be used judiciously, and you must
% 	increase the connection timeout setting on the web server.
% 	<p>
% 	The next example demonstrates an alternative to Comet called \"continuations\"
% 	that achieves similar functionality in a slightly different way and has different
% 	advantages and disadvantages.
% 	";
% 
% notes(web_nbe_6_2) ->
% 	"
% 	A Continuation in Nitrogen allows you to start a process on the server and instruct
% 	the client to continuously poll the server until that process is done. The wf:continue
% 	function also accepts a polling interval and a timeout interval, allowing you to 
% 	kill off the spawned process if it doesn't complete in time.
% 	<p>
% 	An application that uses Continuations will get new data slightly slower than an
% 	application that uses Comet, and will likely see more requests. But the advantage
% 	is that a browser will not monopolize the connection to the server, and the connection
% 	timeout can remain low. 
% 	<p>
% 	In this example, we again create a counter that updates once per second. If you open
% 	Firebug, you will see many more requests being served than in the previous example.
% 	";
% 
% notes(web_nbe_6_3) ->
% 	"
% 	Windex is an experimental new technology that allows you to place Nitrogen applications
% 	into another web pages. This is done by shuttling all postbacks through dynamic script
% 	tags, rather than through Ajax calls like normal. There are cross site scripting security
% 	concerns around this, so if you expose an application through Windex, as a general rule
% 	do not rely on wf:session, wf:role, or wf:user for any sort of authentication. In fact,
% 	you should always assume that the user is anonymous and possibly malicous.
% 	<p>
% 	If that hasn't scared you away, then take a look at this example, it allows you
% 	to update a \"virtual whiteboard\", and uses Comet to push updates to anybody else looking
% 	at the whiteboard.
% 	<p>
% 	Now, for the interesting part, you can download the Windex version of this application
% 	as an HTML page, save it to your local computer, open it in your web browser, and the application
% 	will continue to work.	
% 	";

notes(_) -> undefined.

make_box(PageModule, PageModule) -> #tablecell { class=box, body=#link { class=selected, url=to_url(PageModule), text=" " }};
make_box(Module, _PageModule) -> #tablecell { class=box, body=#link { url=to_url(Module), text=" " }}.	

get_previous(PageModule) -> get_previous(PageModule, pages()).
get_previous(PageModule, [X, PageModule|_]) -> X;
get_previous(PageModule, [_|Rest]) -> get_previous(PageModule, Rest);
get_previous(_, []) -> undefined.
	
get_next(PageModule) -> get_next(PageModule, pages()).
get_next(PageModule, [PageModule, X|_]) -> X;
get_next(PageModule, [_|Rest]) -> get_next(PageModule, Rest);
get_next(_, []) -> undefined.

to_url(PageModule) when is_atom(PageModule) -> [$/|to_url(wf:to_list(PageModule))];
to_url([]) -> [];
to_url([$_|T]) -> [$/|to_url(T)];
to_url([H|T]) -> [H|to_url(T)].

get_source(undefined) -> "";
get_source(web_nbe_3_1) -> [get_source_inner(X) || X <- [web_nbe_3_1, "./src/nbe/click_for_message.inc", click_for_message]];
get_source(web_nbe_3_2) -> [get_source_inner(X) || X <- [web_nbe_3_2, "./src/nbe/backwards_alert.inc", backwards_alert]];
get_source(web_nbe_3_3) -> [get_source_inner(X) || X <- [web_nbe_3_3, "./src/nbe/is_a_z.inc", is_a_z]];
get_source(Module) ->	get_source_inner(Module).

get_source_inner(Module) when is_atom(Module) ->
	CompileInfo = Module:module_info(compile),
	PathToModule = proplists:get_value(source, CompileInfo),
	get_source_inner(PathToModule);
	
get_source_inner(PathToModule) when is_list(PathToModule) ->
	{ok, B} = file:read_file(PathToModule),
	[
	"<pre name='code' class='erlang:nocontrols'>",
	replacements(wf:to_list(B) ++ "\r\n\r\n\r\n\r\n"),
	"</pre>"
	].
	
replacements([]) -> [];
replacements([$<|T]) -> "&lt;" ++ replacements(T);
% replacements([$>|T]) -> "&gt;" ++ replacements(T);
% replacements([$\t|T]) -> "&nbsp;&nbsp;" ++ replacements(T);
% replacements([$\s|T]) -> "&nbsp;" ++ replacements(T);
% replacements([$\n|T]) -> "<BR>" ++ replacements(T);
% replacements([$\r,$\n|T]) -> "<BR>" ++ replacements(T);
replacements([H|T]) -> [H|replacements(T)].
