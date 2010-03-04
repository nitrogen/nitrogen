-module (web_contribute).
-include_lib ("nitrogen/include/wf.hrl").
-compile(export_all).

main() -> #template { file="./templates/onecolumn.html", bindings=[
	{'Group', code},
	{'Item', contribute}
]}.

title() -> "Contribute".
headline() -> "Contribute".

body() -> [
	#h3 { text="How to Contribute" },
	#p{},
	"
		Stay tuned for more information on how to contribute to Nitrogen. 
	"
].

% Taavi Talvik 
%	- Patch for cookie expiration code.

% John Dragos 
%	- Windows startup scripts.

% Martin S. (Zeit_Geist on Twitter) 
% - Suggestion to simplify wf_platform logic.
