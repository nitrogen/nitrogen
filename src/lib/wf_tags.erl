% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% Contributions from Tom McNulty (tom.mcnulty@cetiforge.com)
% See MIT-LICENSE for licensing information.

-module (wf_tags).
-author('tom.mcnulty@cetiforge.com').
-include ("wf.inc").
-export ([emit_tag/2, emit_tag/3]).

%%%  Empty tags %%%

emit_tag(TagName, Props) ->
	STagName = wf:to_list(TagName),
	[
		"<",
		STagName,
		write_props(Props),
		"/>"
	].
	
%%% Tags with child content %%%
    
emit_tag(TagName, Content, Props) ->
	STagName = wf:to_list(TagName),
	[
		"<", 
		STagName, 
		write_props(Props), 
		">", 
	 	Content,
	 	"</", 
		STagName, 
		">"
	].    
    
%%% Property display functions %%%
    
write_props(Props) ->
    lists:map(fun display_property/1, Props).            
     
display_property({Prop, V}) when is_atom(Prop) ->
    display_property({atom_to_list(Prop), V});
    
display_property({_, []}) -> "";    
    
display_property({Prop, Value}) when is_integer(Value); is_atom(Value) ->
	[" ", Prop, "='", wf:to_list(Value), "'"];
    
display_property({Prop, Value}) when is_binary(Value); ?IS_STRING(Value) ->
	[" ", Prop, "='", Value, "'"];

display_property({Prop, Values}) ->
	StrValues = [wf:to_list(X) || X <- Values],
	[" ", Prop, "='", string:strip(string:join(StrValues, " ")), "'"].

