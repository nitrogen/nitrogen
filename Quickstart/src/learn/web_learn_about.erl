-module (web_learn_about).
-include_lib ("nitrogen/include/wf.hrl").
-compile(export_all).

main() -> #template { file="./templates/onecolumn.html", bindings=[
	{'Group', learn},
	{'Item', about}
]}.

title() -> "About Nitrogen".
headline() -> "About Nitrogen".

body() -> [
	"
		Nitrogen was created by <a href='http://rklophaus.com'>Rusty Klophaus</a>. 
		It is in active development, and is available for use under the MIT License.
		<p>
		Email: <a href='mailto:nitrogenframework@gmail.com'>NitrogenFramework@gmail.com</a>		
	",	
	#h3 { text="Core Team" },
	"
		The Nitrogen core team consists of:
		<p>
		<ul> 
		<li>Rusty Klophaus (<a href='http://rklophaus.com'>www</a>, <a href='http://twitter.com/rklophaus'>twitter</a>) 
		<li>Jon Gretar Borgthorsson (<a href='http://medevyoujane.com/'>www</a>, <a href='http://twitter.com/jongretar'>twitter</a>)
		<li>Martin Scholl (<a href='http://twitter.com/zeitgeist'>twitter</a>) 
		</ul>
	",	
	#h3 { text="Shout Outs" },
	"Thanks to the many people who have helped make Nitrogen better, including:",
	#p{},
	#list { body=[
		#listitem { text="Dave Peticolas" },
		#listitem { text="Joel Reymont (twitter: @wagerlabs)"  },
		#listitem { text="Chris Williams (twitter: @voodootikigod)" },
		#listitem { text="Taavi Talvik" },
		#listitem { text="John Dragos" }
	]}
].

% Taavi Talvik 
%	- Patch for cookie expiration code.

% John Dragos 
%	- Windows startup scripts.

% Martin S. (Zeit_Geist on Twitter) 
% - Suggestion to simplify wf_platform logic.
