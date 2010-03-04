-module (web_learn).
-include_lib ("nitrogen/include/wf.hrl").
-compile(export_all).

main() -> #template { file="./templates/onecolumn.html", bindings=[
	{'Group', learn},
	{'Item', learn}
]}.

title() -> "Introducing Nitrogen".
headline() -> "Introducing Nitrogen".

body() -> [
	#p{},
	"
		Nitrogen brings cutting-edge web development to Erlang.
	",
	#p{},
	
	#h3 { text="Event-Driven Development" },
	#p{},
	"
		Nitrogen uses an event-driven model built on top of Erlang pattern matching. Nitrogen
		allows you to tag elements with any Erlang term, and then act on the tag in server-side
		code when the user clicks on, hovers over, or otherwise interacts with the element. 
		Catching the event is as simple as writing an Erlang function.
	",
	
	#h3 { text="Brainlessly Easy Ajax" },
	#p{},
	"
		Nitrogen allows you to update or replace a section of your page using Ajax in just one 
		line of code. Most importantly, Nitrogen lets you use the same consistent syntax to both
		build AND update the page.
	",
	
	#h3 { text="Ridiculously Simple Comet" },
	#p{},
	"
		Nitrogen includes Comet support, allowing you to build interactive web applications
		that push data to the browser. By simply wrapping your function call
		in one line of code, you can turn a synchronous function into a long-running asynchronous function.
	",
	
	#h3 { text="Complex Interfaces: Dragging, Dropping, and Sorting" },
	#p{},
	"
		With Nitrogen, you can let a user interact with your application in complex ways.
		Nitrogen lets you tag the draggable, droppable, or sortable elements, and then respond to 
		the resulting events in server-side code as easily as you would respond to a click. 
	",
	
	#h3 { text="Flexible Templating" },
	#p{},
	"
		Nitrogen includes a simple but powerful template system, allowing you to define
		a consistent style for your application. You can add headers, footers, and parameterized 
		plugins to your page using a simple callout mechanism.
	",
	
	#h3 { text="Data Binding" },
	#p{},
	"
		Nitrogen leverages the power of Erlang data structures and pattern matching to 
		enable powerful one-way databinding. Data binding support makes it easy to display repeated
		documents like blog posts or comments.
	",
	
	#h3 { text="Erlang Power" },
	#p{},
	"
		Nitrogen brings all of the advantages of Erlang to web application development,
		including hot code swapping, stability, and scalability.
	",
	
	#hr {},
	
	#h2 { text="Technology Stack" },
	#h3 { text="Mac, Linux, Windows, etc." },
	#p{},
	"	
		Any platform that can	run Erlang can also run Nitrogen, including Mac, Linux, Unix, and Windows (among others). 
		Nitrogen does not contain	any platform specific code. 
	",
	
	#h3 { text="Mochiweb, Yaws, or Inets for Serving" },
	#p{},
	"
		Nitrogen supports the three most popular Erlang web servers equally: Mochiweb, Yaws, and Inets. 
		Nitrogen abstracts out the server specific code, meaning that you write your application on 
		one http server and seamlessly transfer to a different http server without changing a thing.
	",
	
	#h3 { text="JQuery/JQuery UI for Javascript and Effects" },
	#p{},
	"
		Nitrogen uses JQuery and the JQuery UI library for client side Javascript.
	"
].

event(_) -> ok.
