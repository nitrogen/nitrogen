-ifndef(wf_inc).
-define(wf_inc, ok).

%%% CONTEXT %%%

% Page Request Information.
-record(page_context, {
	name=page,   % 'page' by default. Can be renamed by Inline Apps.
	series_id,   % A unique ID assigned to the first request which stays constant on repeated requests.
	module,      % The requested page module
	path_info,   % Any extra info passed with the request
	async_mode={poll, 1000} % {poll, Interval} or comet 
}).

% Event Information. A serialized version of this record
% is sent by the browser when an event is called.
-record(event_context, {
	module,     % The module that should handle the event
  tag,        % An Erlang term that is passed along with the event
  type,       % The type of event postback, comet, continuation, upload
  trigger,    % The element id that triggered the event
  target      % The element id that "receives" the event
}).

% Handlers Context-
% Handlers are used to encapsulate Nitrogen's behaviour, and
% to allow other frameworks to substitute their own behaviour.
% These are set in wf_context:make_context/1
-record(handler_context, {
	name,    % The name of a handler. See wf_context for a list.
	module,     % A module that provides the logic for a handler. This can be substituted by your app.
	state       % An Erlang Struct that holds the state of a module.
}).

-record(context, {
	% Transient Information
	type,                % Either first_request, postback_request, or static_file
	request_bridge,      % Holds the simple_bridge request object
	response_bridge,     % Holds the simple_bridge response object
	dom_paths=undefined, % A list of dom paths corresponding to elements in the browser. Used during the wire process.
	current_path=[],     % Used during rendering to hold the path of whatever is currently getting rendered. 
	data=[],             % Holds whatever the page_module:main/1 method returns: HTML, Elements, Binary, etc..
	queued_actions=[],   % List of actions queued in main/1, event/2, or when rendering elements.	

	% These are all serialized, sent to the browser
	% and de-serialized on each request.
	page_context,
	event_context,
	handler_list
}).

%%% LOGGING %%%
-define(PRINT(Var), error_logger:info_msg("DEBUG: ~p:~p~n~p~n  ~p~n", [?MODULE, ?LINE, ??Var, Var])).
-define(LOG(Msg, Args), error_logger:info_msg(Msg, Args)).
-define(DEBUG, error_logger:info_msg("DEBUG: ~p:~p~n", [?MODULE, ?LINE])).

%%% GUARDS %%%
-define (IS_STRING(Term), (is_list(Term) andalso Term /= [] andalso is_integer(hd(Term)))).

%%% FRAMEWORK %%%

%%% Elements %%%
-define(ELEMENT_BASE(Module), is_element=is_element, module=Module, id, actions, show_if=true, class="", style="").
-record(elementbase, {?ELEMENT_BASE(undefined)}).
-record(template, {?ELEMENT_BASE(element_template), file, bindings=[] }).
-record(function, {?ELEMENT_BASE(element_function), function=fun() -> [] end}).
-record(body, {?ELEMENT_BASE(element_body), title="", body=[]}).
-record(h1, {?ELEMENT_BASE(element_h1), text="", html_encode=true}).
-record(h2, {?ELEMENT_BASE(element_h2), text="", html_encode=true}).
-record(h3, {?ELEMENT_BASE(element_h3), text="", html_encode=true}).
-record(h4, {?ELEMENT_BASE(element_h4), text="", html_encode=true}).
-record(list, {?ELEMENT_BASE(element_list), numbered=false, body=[]}).
-record(listitem, {?ELEMENT_BASE(element_listitem), body=[], text="", html_encode=true}).
-record(br, {?ELEMENT_BASE(element_br) }).
-record(hr, {?ELEMENT_BASE(element_hr) }).
-record(p, {?ELEMENT_BASE(element_p), body=""}).
-record(label, {?ELEMENT_BASE(element_label), text="", html_encode=true}).
-record(value, {?ELEMENT_BASE(element_value), text="", html_encode=true}).
-record(link, {?ELEMENT_BASE(element_link), text="", body="", html_encode=true, url="javascript:", postback}).
-record(error, {?ELEMENT_BASE(element_error), text="", html_encode=true}).
-record(span, {?ELEMENT_BASE(element_span), text="", html_encode=true}).
-record(button, {?ELEMENT_BASE(element_button), text="Button", html_encode=true, postback}).
-record(literal, {?ELEMENT_BASE(element_literal), text="", html_encode=true}).
-record(textbox, {?ELEMENT_BASE(element_textbox), text="", html_encode=true, next, postback}).
-record(hidden, {?ELEMENT_BASE(element_hidden), text="", html_encode=true}).
-record(textarea, {?ELEMENT_BASE(element_textarea), text="", html_encode=true}).
-record(datepicker_textbox, {?ELEMENT_BASE(element_datepicker_textbox), text="", next, html_encode=true, validators=[], options = [{dateFormat, "yy-mm-dd"}] }).
-record(dropdown, {?ELEMENT_BASE(element_dropdown), options=[], html_encode=true, postback, value}).
-record(option, { text="", value="", selected=false }).
-record(checkbox, {?ELEMENT_BASE(element_checkbox), text="", html_encode=true, checked=false, postback}).
-record(radiogroup, {?ELEMENT_BASE(element_radiogroup), body=[]}).
-record(radio, {?ELEMENT_BASE(element_radio), text="", html_encode=true, value, name, checked=false, postback}).
-record(password, {?ELEMENT_BASE(element_password), text="", html_encode=true, next, postback}).
-record(panel, {?ELEMENT_BASE(element_panel), body=""}).
-record(spinner, {?ELEMENT_BASE(element_spinner), image="/nitrogen/spinner.gif"}).
-record(image, {?ELEMENT_BASE(element_image), image="", alt}).
-record(rounded_panel, {?ELEMENT_BASE(element_rounded_panel), body="", color=gray}).
-record(lightbox, {?ELEMENT_BASE(element_lightbox), body="" }).
-record(table, {?ELEMENT_BASE(element_table), rows}).
-record(tablerow, {?ELEMENT_BASE(element_tablerow), cells}).
-record(tableheader, {?ELEMENT_BASE(element_tableheader), text="", html_encode=true, body="", align="left", valign="middle", colspan=1, rowspan=1}).
-record(tablecell, {?ELEMENT_BASE(element_tablecell), text="", html_encode=true, body="", align="left", valign="middle", colspan=1, rowspan=1}).
-record(singlerow, {?ELEMENT_BASE(element_singlerow), cells}). 
-record(file, {?ELEMENT_BASE(element_file), file}).
-record(flash, {?ELEMENT_BASE(element_flash)}).
-record(placeholder, {?ELEMENT_BASE(element_placeholder), body=[]}).
-record(bind, {?ELEMENT_BASE(element_bind), data=[], map=[], transform, acc=[], body=[], empty_body=[]}).
-record(sortblock, {?ELEMENT_BASE(element_sortblock), tag, items=[], group, connect_with_groups=none, handle }).
-record(sortitem, {?ELEMENT_BASE(element_sortitem), tag, body=[] }).
-record(draggable, {?ELEMENT_BASE(element_draggable), tag, body=[], group, handle, clone=true, revert=true}).
-record(droppable, {?ELEMENT_BASE(element_droppable), tag, body=[], accept_groups=all, active_class=active, hover_class=hover}).
-record(gravatar, {?ELEMENT_BASE(element_gravatar), email="", size="80", rating="g", default=""}).
-record(inplace_textbox, {?ELEMENT_BASE(element_inplace_textbox), tag, text="", html_encode=true, start_mode=view, validators=[]}).
-record(wizard, {?ELEMENT_BASE(element_wizard), tag, titles, steps }).
-record(upload, {?ELEMENT_BASE(element_upload), tag, show_button=true, button_text="Upload" }).

%%% Actions %%%
-define(ACTION_BASE(Module), is_action=is_action, module=Module, trigger, target, actions, show_if=true).
-record(actionbase, {?ACTION_BASE(undefined)}).
-record(wire, {?ACTION_BASE(action_wire)}).
-record(continue, {?ACTION_BASE(action_continue), function, delegate, tag, interval=100, timeout=20000}).
-record(update, {?ACTION_BASE(action_update), type=update, elements=[]}).
-record(async, {?ACTION_BASE(action_async), pool=default, scope=local, function, dying_message}).
-record(set, {?ACTION_BASE(action_set), value}).
-record(redirect, {?ACTION_BASE(action_redirect), url}).
-record(event, {?ACTION_BASE(action_event), type=click, delay=0, postback, delegate}).
-record(validate, {?ACTION_BASE(action_validate), on=submit, success_text=" ", validators, attach_to }).
-record(validation_error, {?ACTION_BASE(action_validation_error), text="" }).
-record(alert, {?ACTION_BASE(action_alert), text=""}).
-record(confirm, {?ACTION_BASE(action_confirm), text="", postback}).
-record(script, {?ACTION_BASE(action_script), script}).
-record(disable_selection, {?ACTION_BASE(action_disable_selection)}).
-record(jquery_effect, {?ACTION_BASE(action_jquery_effect), type, effect, speed, options=[], class, easing}).
-record(show, {?ACTION_BASE(action_show), effect=none, options=[], speed=500}).
-record(hide, {?ACTION_BASE(action_hide), effect=none, options=[], speed=500}).
-record(appear, {?ACTION_BASE(action_appear), speed=500}).
-record(fade, {?ACTION_BASE(action_fade), speed=500}).
-record(effect, {?ACTION_BASE(action_effect), effect=none, options=[], speed=500}).
-record(toggle, {?ACTION_BASE(action_toggle), effect=none, options=[], speed=500}).
-record(add_class, {?ACTION_BASE(action_add_class), class=none, speed=0}).
-record(remove_class, {?ACTION_BASE(action_remove_class), class=none, speed=0}).
-record(animate, {?ACTION_BASE(action_animate), options=[], speed=500, easing=swing}).
-record(buttonize, {?ACTION_BASE(action_buttonize)}).
-record(comet_start, {?ACTION_BASE(action_comet_start)}).

%%% Validators %%%
-define(VALIDATOR_BASE(Module), ?ACTION_BASE(Module), text="Failed.").
-record(validatorbase, {?VALIDATOR_BASE(undefined)}).
-record(is_required, {?VALIDATOR_BASE(validator_is_required)}).
-record(is_email, {?VALIDATOR_BASE(validator_is_email)}).
-record(is_integer, {?VALIDATOR_BASE(validator_is_integer)}).
-record(min_length, {?VALIDATOR_BASE(validator_min_length), length}).
-record(max_length, {?VALIDATOR_BASE(validator_max_length), length}).
-record(confirm_password, {?VALIDATOR_BASE(validator_confirm_password), password}).
-record(custom, {?VALIDATOR_BASE(validator_custom), function, tag }).
-record(js_custom, {?VALIDATOR_BASE(validator_js_custom), function, args="{}" }).


-endif.
