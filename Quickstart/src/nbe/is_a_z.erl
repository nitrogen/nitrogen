% ./src/nbe/is_a_z.erl
-module (is_a_z).
-include ("wf.inc").
-include ("is_a_z.inc").
-compile(export_all).

render_validator(TriggerPath, TargetPath, Record)  ->
	Text = wf_utils:js_escape(Record#is_a_z.text),
	
	% Create #custom record...
	F = fun(_Tag, Value) ->
		hd(string:to_upper(Value)) == $A 
		andalso
		hd(string:to_upper(lists:reverse(Value))) == $Z
	end,
	Custom = #custom { function=F, text = Text, tag=Record },
	
	% Create #js_custom record...
	JSFunction = "function(value) { 
		return value[0].toUpperCase() == 'A' &&
		       value[value.length-1].toUpperCase() == 'Z';
	  }
	",
	JSCustom = #js_custom { function=JSFunction, text=Text },
	
	% Render both records...
	[
	validator_custom:render_validator(TriggerPath, TargetPath, Custom),
	validator_js_custom:render_validator(TriggerPath, TargetPath, JSCustom)
	].