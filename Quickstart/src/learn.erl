-module (learn).
-include_lib ("nitrogen/include/wf.hrl").
-compile(export_all).

main() -> #template { file="./templates/grid.html" }.

title() -> "Learn More".

headline() -> "Learn More".

layout() ->
    #container_12 { body=[
        #grid_12 { class=header, body=common:header(learn) },
        #grid_clear {},

        #grid_10 { alpha=true, omega=true, prefix=1, suffix=1, class=headline, body=headline() },
        #grid_clear {},

        #grid_5 { alpha=true, prefix=1, class=pad_right, body=left() },
        #grid_5 { omega=true, suffix=1, body=right() },
        #grid_clear {},

        #grid_12 { body=common:footer() }
    ]}.
        

left() -> 
    [
        #p{},
        "
        Nitrogen brings interactive web applications to Erlang.
        ",
        #p{},
        
        #h2 { text="Event-Driven Development" },
        #p{},
        "
        Nitrogen uses an event-driven model built on top of Erlang pattern matching. Nitrogen
        allows you to tag elements with any Erlang term, and then act on the tag in server-side
        code when the user clicks on, hovers over, or otherwise interacts with the element. 
        Catching the event is as simple as writing an Erlang function.
        ",
        
        #h2 { text="Brainlessly Easy Ajax" },
        #p{},
        "
        Nitrogen allows you to update or replace a section of your page using Ajax in just one 
        line of code. Most importantly, Nitrogen lets you use the same consistent syntax to both
        build AND update the page.
        ",
        
        #h2 { text="Ridiculously Simple Comet" },
        #p{},
        "
        Nitrogen includes Comet support, allowing you to build interactive web applications
        that push data to the browser. By simply wrapping your function call
        in one line of code, you can turn a synchronous function into a long-running asynchronous function.
        ",
        
        #h2 { text="Complex Interfaces: Drag/Drop/Sort" },
        #p{},
        "
        With Nitrogen, you can let a user interact with your application in complex ways.
        Nitrogen lets you tag the draggable, droppable, or sortable elements, and then respond to 
        the resulting events in server-side code as easily as you would respond to a click. 
        ",
        
        #h2 { text="Flexible Templating" },
        #p{},
        "
        Nitrogen includes a simple but powerful template system, allowing you to define
        a consistent style for your application. You can add headers, footers, and parameterized 
        plugins to your page using a simple callout mechanism.
        ",
        
        #h2 { text="Data Binding" },
        #p{},
        "
        Nitrogen leverages the power of Erlang data structures and pattern matching to 
        enable powerful one-way databinding. Data binding support makes it easy to display repeated
        documents like blog posts or comments.
        ",
        
        #h2 { text="Erlang Power" },
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

right() ->
    [
        #h2 { text="Documentation" },
        
        "
        <p>
        Nitrogen documentation is available for <a
        href='/doc/index.html'>browsing online</a>.  It is also
        included in the source code under <i>/doc/html</i>, and
        is packaged in all binary downloads.
        
        <p>
        Read <a href='/whatsnew'>What's New in Nitrogen 2.x &raquo;</a>
        ",
        
        #h2 { text="About Nitrogen" },

	"
        <p>
        Nitrogen was created by <a href='http://rklophaus.com'>Rusty
        Klophaus</a> (@rklophaus).  It is in active development, and
        is available for use under the MIT License.
        ",

        "
        <p>
        Twitter: <a href='http://twitter.com/nitrogenproject'>@nitrogenproject</a>
        ",

	#h2 { text="Thanks!" },

        #p{},
	"Thanks to the many people who have helped make Nitrogen better, including:",

        #p{},
        #list { body=[
            #listitem { text="Chris Williams (@voodootikigod)" },
            #listitem { text="Joel Reymont (@wagerlabs)" },
            #listitem { text="Tom McNulty" },
            #listitem { text="Martin Scholl (@zeit_geist)" },
            #listitem { text="Dave Peticolas" },
            #listitem { text="Jon Gretar Borgthorsson (@jongretar)" },
            #listitem { text="Dan Bravender (@dbravender)" },
            #listitem { text="Taavi Talvik" },
            #listitem { text="Torbjorn Tornkvist (@kruskakli)" },
            #listitem { text="Marius A. Eriksen (@marius)" },
            #listitem { text="Michael Mullis" },
            #listitem { text="John Dragos" },
            #listitem { text="Benjamin Nortier (@bjnortier)" },
            #listitem { text="Jay Doane" },
            #listitem { text="Robert Schonberger" },
            #listitem { text="Yurii Rashkovskii (@yrashk)" },
            #listitem { text="Ville Koivula" },
            #listitem { text="Manuel Duran Aguete" },
            #listitem { text="Jesse Gumm" },
            #listitem { text="Jan-Felix Wittmann" },
            #listitem { text="Martin Sivak" }
        ]}
    ].

event(_) -> ok.
