-module (web_reference_validators).
-include_lib ("nitrogen/include/wf.hrl").
-compile(export_all).

main() -> 
 	PathInfo = wf:get_path_info(),
	Bindings = [{'Group', reference}, {'Item', validators}],
	case PathInfo of 
		[] -> 
			put(title, "Nitrogen Validator Reference"),
			#template { file="./templates/twocolumn.html", bindings=Bindings };
		_ -> 
			Terms = read_file("./reference/validators", PathInfo, "txt"),
			case Terms of 
				[Term] -> 
					Term1 = [{Key, trim(Value)} || {Key, Value} <- Term],
					put(title, proplists:get_value(title, Term1)),
					put(term, Term1),
					#template { file="./templates/onecolumn.html", bindings=Bindings };
				_ -> 
					[]
			end
	end.
	
title() -> get(title).
headline() -> get(title).

get_binding_map() -> {link@url, link@text}.
get_binding_body() -> [
	#link { id=link },
	#br {}
].

column1() -> 
 	RequiredAndSize = [
		{ "/web/reference/validators/is_required", "Required" },
		{ "/web/reference/validators/min_length", "Min Length" },
		{ "/web/reference/validators/max_length", "Max Length" }
	],
	
	[
		#h3 { text="Required and Data Size" },
		#bind { data=RequiredAndSize, map=get_binding_map(), body=get_binding_body() }
	].	

column2() -> 
	Format = [
		{ "/web/reference/validators/is_email", "Is Email" },
		{ "/web/reference/validators/is_integer", "Is Integer" },
		{ "/web/reference/validators/confirm_password", "Confirm Passwords" }
	],
	
	Other = [
		{ "/web/reference/validators/js_custom", "Custom Client Side (JS)" },
		{ "/web/reference/validators/custom", "Custom Server Side (Erlang)" }
	],
	
	[
		#h3 { text="Data Format" },
		#bind { data=Format, map=get_binding_map(), body=get_binding_body() },
		
		#h3 { text="Other" },
		#bind { data=Other, map=get_binding_map(), body=get_binding_body() }
	].

body() ->
	Term = get(term),
	
	#bind { 
		data=[Term],
		map=[{description, description@text}, {see_also, see_also@data}, {usage, usage@text}, {attributes, attributes@data}],
		body=[
			#p{},
			#span { class=reference_description, id=description, html_encode=false },
			#p{},
			#h2{ text="Usage" },
			#span { class=reference_usage, id=usage, html_encode=false },
			#h2 { text="Attributes" },
			#bind { id=attributes, data=[], map={name@text, datatype@text, description@text}, 
				body=[
					#p{},
					#span { class=reference_attribute_name, id=name },
					#span { class=reference_attribute_description, id=description, html_encode=false },
					#span { class=reference_attribute_datatype, id=datatype }
				],
				empty_body="<p>No attributes."
			}
		]
	}.


event(_) -> ok.

read_file(Directory, File, Ext) ->
	try
		{ok, Terms} = file:consult(Directory ++ "/" ++ File ++ "." ++ Ext),
		Terms
	catch 
		Type : Error -> ?PRINT({error, Type, Error, File}), []
	end.

%% trim/1 - Trim leading and trailing spaces, tabs, and newlines.
trim(Value) -> lists:reverse(trim1(lists:reverse(trim1(Value)))).
trim1([]) -> [];
trim1([H|T]) when H==$\s orelse H==$\t orelse H==$\n -> trim1(T);
trim1([H|T]) -> [H|T].
