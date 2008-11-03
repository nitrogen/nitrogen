-module (wf_bind).
-export ([
	set/2,
	bind/2,
	reverse_bind/1,
	reverse_bind/2
]).

set(Element, Value) ->
	Value1 = wf_utils:js_escape(wf:to_list(Value)),
	wf:wire(Element, wf:f("wf_set_value(me, \"~s\");", [Value1])).
	

bind(T, Record) when is_tuple(T), is_tuple(Record) -> 
	bind(2, T, Record).
	
bind(Pos, T, Record) when Pos =< size(T) ->
	case element(Pos, T) of 
		undefined -> 
			ok;
		Element ->
			set(Element, element(Pos, Record))
	end,
	bind(Pos + 1, T, Record);
	
bind(_, _, _) -> ok.


reverse_bind(T) when is_tuple(T) ->
	Head = element(1, T),
	Tail = lists:duplicate(size(T) - 1, undefined),
	Record = list_to_tuple([Head|Tail]),
	reverse_bind(T, Record).
	
reverse_bind(T, Record) when is_tuple(T), is_tuple(Record) -> 
	reverse_bind(2, T, Record). 

reverse_bind(Pos, T, Record) when Pos =< size(T) ->
	Value = case element(Pos, T) of
		undefined -> 
			element(Pos, Record);
		Element ->
			[X] = wf:q(Element),
			X
	end,
	reverse_bind(Pos + 1, T, setelement(Pos, Record, Value));
	
reverse_bind(_, _, Record) -> Record.