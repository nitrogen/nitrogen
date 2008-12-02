% Nitrogen Web Framework for Erlang
% Copyright (c) 2008 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (action_dragdrop).
-include ("wf.inc").
-compile(export_all).

render_action(TriggerPath, TargetPath, Record) -> 
	% JQUERY SPECIFIC CODE.
	Helper = case Record#dragdrop.clone of
		true -> clone;
		false -> original
	end,
	
	% Make drag options.
	DragOptions = wf:f("{ helper: '~s', revert: '~s' }", [
		Helper, 
		Record#dragdrop.revert
	]),
		
	% Make drop options.
	DropOptions = wf:f("{ drop : function() { ~s }, activeClass: '~s', hoverClass: '~s' }", [
		action_event:make_postback(Record#dragdrop.postback, dragdrop, TriggerPath, TargetPath, undefined),
		Record#dragdrop.active_class,
		Record#dragdrop.hover_class
	]),
	
	wf:f("wf_dragdrop(obj('~s'), ~s, obj('~s'), ~s);", [
		TriggerPath,
		DragOptions,
		TargetPath,
		DropOptions
	]).
