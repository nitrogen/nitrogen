% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_flash).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, flash).
	
render_element(_HtmlID, _Record, Context) -> 
	Terms = #panel { 
		id=flash,
		class=flash_container
	},
	{ok, Context1} = wff:state(has_flash, true, Context),
	{ok, Terms, Context1}.

% render/0 - Convenience methods to place the flash element on a page from a template.
render() -> #flash{}.
render(Context) -> {ok, #flash{}, Context}.

update(Context) ->
	% TODO - Stifle flash when we are redirecting.
	HasFlash = wff:state(has_flash, Context),
	case HasFlash of
		true -> 
			{ok, Flashes, Context1} = get_flashes(Context),
			wff:insert_bottom(flash, Flashes, Context1);
	  _ -> 
			{ok, Context}
	end.

add_flash(Term, Context) ->
	Flashes = case wff:session(flashes, Context) of
		undefined -> [];
		X -> X
	end,
	{ok, _NewContext} = wff:session(flashes, [Term|Flashes], Context).

get_flashes(Context) -> 
	% Create terms for an individual flash...
	F = fun(X) ->
		FlashID = wff:temp_id(),
		InnerPanel = #panel { class=flash, actions=#show { target=FlashID, effect=blind, speed=400 }, body=[
			#link { class=flash_close_button, text="Close", actions=#event { type=click, target=FlashID, actions=#hide { effect=blind, speed=400 } } },
			#panel { class=flash_content, body=X }
		]},
		#panel { id=FlashID, style="display: none;", body=InnerPanel}
	end,
	
	% Get flashes, and clear session...
	Flashes = case wff:session(flashes, Context) of 
		undefined -> [];
		Other -> Other
	end,	
	{ok, Context1} = wff:session(flashes, [], Context),
	
	% Return list of terms...
	Flashes1 = [F(X) || X <- lists:reverse(Flashes)],
	{ok, Flashes1, Context1}.