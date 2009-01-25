-module (web_reference_actions).
-include ("wf.inc").
-compile(export_all).

main() -> 
 	PathInfo = wf:get_path_info(),
	Bindings = [{'Group', reference}, {'Item', actions}],
	case PathInfo of 
		[] -> 
			put(title, "Nitrogen Action Reference"),
			#template { file="./wwwroot/twocolumn.html", bindings=Bindings };
		_ -> 
			Terms = read_file("./reference/actions", PathInfo, "txt"),
			case Terms of 
				[Term] -> 
					Term1 = [{Key, trim(Value)} || {Key, Value} <- Term],
					put(title, proplists:get_value(title, Term1)),
					put(term, Term1),
					#template { file="./wwwroot/onecolumn.html", bindings=Bindings };
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
 	Effects = [
		{ "/web/reference/actions/show", "Show" },
		{ "/web/reference/actions/hide", "Hide" },
		{ "/web/reference/actions/appear", "Appear" },
		{ "/web/reference/actions/fade", "Fade" },
		{ "/web/reference/actions/animate", "Animate" },
		{ "/web/reference/actions/toggle", "Toggle" },
		{ "/web/reference/actions/effect", "Effect" },
		{ "/web/reference/actions/add_class", "Add Class" },
		{ "/web/reference/actions/remove_class", "Remove Class" }
	],
	
	[
		#h3 { text="Effects" },
		#bind { data=Effects, map=get_binding_map(), body=get_binding_body() }
	].	

column2() -> 
	Feedback = [
		{ "/web/reference/actions/alert", "Alert" },
		{ "/web/reference/actions/confirm", "Confirm" }
	],
	
	Other = [
		{ "/web/reference/actions/base", "(Base Action)" },
		{ "/web/reference/actions/event", "Event" },
		{ "/web/reference/actions/script", "Script" },
		{ "/web/reference/actions/validate", "Validate" }
	],
	
	[
		#h3 { text="Feedback" },
		#bind { data=Feedback, map=get_binding_map(), body=get_binding_body() },
		
		#h3 { text="Other" },
		#bind { data=Other, map=get_binding_map(), body=get_binding_body() }
	].

body() ->
	Term = get(term),
	TransformSeeAlso = fun(SeeAlso, Acc) -> 
		SeeAlso1 = wf:to_list(SeeAlso),
		{{SeeAlso1, SeeAlso1}, Acc, []} 
	end,
	
	#bind { 
		data=[Term],
		map=[{description, description@text}, {see_also, see_also@data}, {usage, usage@text}, {attributes, attributes@data}, {events, events@data}],
		body=[
			#p{},
			#span { class=reference_description, id=description, html_encode=false },
			#p{},
			#span { class=reference_see_also_label, text="See also: " },
			#bind { id=see_also, data=[], map={link@text, link@url}, transform=TransformSeeAlso,
				body=[#link { class=reference_see_also_link, id=link }, "&nbsp;"],
				empty_body="-"
			},
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
			},
			#h2 { text="Events" },
			#bind { id=events, data=[], map={name@text, description@text}, 
				body=[
					#p{},
					#span { class=reference_event_name, id=name },
					#span { class=reference_event_description, id=description, html_encode=false },
					#br{}
				],
				empty_body="<p>No events."
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