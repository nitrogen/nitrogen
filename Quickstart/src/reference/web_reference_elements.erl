-module (web_reference_elements).
-include ("wf.inc").
-compile(export_all).

main() -> 
 	PathInfo = wf:get_path_info(),
	Bindings = [{'Group', reference}, {'Item', elements}],
	case PathInfo of 
		[] -> 
			put(title, "Nitrogen Element Reference"),
			#template { file="./wwwroot/twocolumn.html", bindings=Bindings };
		_ -> 
			Terms = read_file("./reference/elements", PathInfo, "txt"),
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
	Layout = [
		{ "/web/reference/elements/template", "Template" },
		{ "/web/reference/elements/panel", "Panel" },
		{ "/web/reference/elements/rounded_panel", "Rounded Panel" },
		{ "/web/reference/elements/span", "Span" },
		{ "/web/reference/elements/lightbox", "Lightbox" },
		{ "/web/reference/elements/wizard", "Wizard" }
	],
	
	Tables = [
		{ "/web/reference/elements/table", "Table" },
		{ "/web/reference/elements/tablerow", "Table Row" },
		{ "/web/reference/elements/tablecell", "Table Cell" },
		{ "/web/reference/elements/tableheader", "Table Header" },
		{ "/web/reference/elements/singlerow", "Single Row Table" }
	],
	
	Other=[
		{ "/web/reference/elements/base", "(Base Element)" },
		{ "/web/reference/elements/bind", "Bind" },
		{ "/web/reference/elements/file", "File" },
		{ "/web/reference/elements/flash", "Flash" },
		{ "/web/reference/elements/label", "Label" },
		{ "/web/reference/elements/value", "Value" },
		{ "/web/reference/elements/draggable", "Draggable" },
		{ "/web/reference/elements/droppable", "Droppable" },
		{ "/web/reference/elements/sortblock", "Sort Block" },
		{ "/web/reference/elements/sortitem", "Sort Item" },
        { "/web/reference/elements/gravatar", "Gravatar"}
	],
	
	[
		#h3 { text="Layout" },
		#bind { data=Layout, map=get_binding_map(), body=get_binding_body() },
		
		#h3 { text="Tables" },
		#bind { data=Tables, map=get_binding_map(), body=get_binding_body() },
				
		#h3 { text="Other" },
		#bind { data=Other, map=get_binding_map(), body=get_binding_body() }
	].	

column2() -> 
	HTML = [
		{ "/web/reference/elements/image", "Image" },
		{ "/web/reference/elements/link", "Link" },
		{ "/web/reference/elements/literal", "Literal" },
		{ "/web/reference/elements/h1", "Heading 1" },
		{ "/web/reference/elements/h2", "Heading 2" },
		{ "/web/reference/elements/h3", "Heading 3" },
		{ "/web/reference/elements/h4", "Heading 4" },
		{ "/web/reference/elements/br", "Break" },
		{ "/web/reference/elements/hr", "Horizontal Rule" },
		{ "/web/reference/elements/p", "Paragraph" }
	],
	
	Forms=[
		{ "/web/reference/elements/button", "Button" },
		{ "/web/reference/elements/textbox", "Textbox" },
		{ "/web/reference/elements/inplace_textbox", "In-Place Textbox" },
		{ "/web/reference/elements/textarea", "Textarea" },
		{ "/web/reference/elements/checkbox", "Checkbox" },
		{ "/web/reference/elements/dropdown", "Dropdown List" },
		{ "/web/reference/elements/option", "Dropdown Option" },
		{ "/web/reference/elements/password", "Password Box" },
		{ "/web/reference/elements/hidden", "Hidden Value" },
		{ "/web/reference/elements/spinner", "AJAX Spinner" }
	],
	
	[
		#h3 { text="HTML" },
		#bind { data=HTML, map=get_binding_map(), body=get_binding_body() },
		
		#h3 { text="Forms" },
		#bind { data=Forms, map=get_binding_map(), body=get_binding_body() }
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
			#span { class=reference_usage, id=usage, html_encode=true },
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