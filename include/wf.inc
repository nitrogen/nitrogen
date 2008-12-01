-ifndef(wf_inc).
-define(wf_inc, ok).

%%% LOGGING %%%
-define(PRINT(Var), error_logger:info_msg("DEBUG: ~p:~p - ~p: ~p~n", [?MODULE, ?LINE, ??Var, Var])).
-define(LOG(Msg, Args), error_logger:info_msg(Msg, Args)).
-define(DEBUG, error_logger:info_msg("DEBUG: ~p:~p~n", [?MODULE, ?LINE])).


%%% EMAIL %%%
-record(email, {from, to, subject, body}).


%%% FRAMEWORK %%%

%%% Elements %%%
-define(ELEMENT_BASE, id, actions, show_if=true, class="", style="").
-record(template, { ?ELEMENT_BASE, file, title="[title]", headline="[headline]", section1, section2, section3, section4, extra }).
-record(body, {?ELEMENT_BASE, title="", body=[]}).
-record(h1, {?ELEMENT_BASE, text="", html_encode=true}).
-record(h2, {?ELEMENT_BASE, text="", html_encode=true}).
-record(h3, {?ELEMENT_BASE, text="", html_encode=true}).
-record(h4, {?ELEMENT_BASE, text="", html_encode=true}).
-record(br, { ?ELEMENT_BASE }).
-record(hr, { ?ELEMENT_BASE }).
-record(p, { ?ELEMENT_BASE }).
-record(label, {?ELEMENT_BASE, text="", html_encode=true}).
-record(value, {?ELEMENT_BASE, text="", html_encode=true}).
-record(link, {?ELEMENT_BASE, text="", html_encode=true, url="javascript:", postback=undefined}).
-record(span, {?ELEMENT_BASE, text="", html_encode=true}).
-record(button, {?ELEMENT_BASE, text="Button", html_encode=true, postback=undefined}).
-record(literal, {?ELEMENT_BASE, text="", html_encode=true}).
-record(textbox, {?ELEMENT_BASE, text="", html_encode=true, next=undefined, postback=undefined}).
-record(hidden, {?ELEMENT_BASE, text="", html_encode=true}).
-record(textarea, {?ELEMENT_BASE, text="", html_encode=true}).
-record(dropdown, {?ELEMENT_BASE, options=[], html_encode=true, postback=undefined, value=undefined}).
-record(option, { text="", value="", selected=false }).
-record(checkbox, {?ELEMENT_BASE, text="", html_encode=true, checked=false, postback=undefined}).
-record(password, {?ELEMENT_BASE, text="", html_encode=true, next=undefined, postback=undefined}).
-record(panel, {?ELEMENT_BASE, body=""}).
-record(spinner, {?ELEMENT_BASE, image="/nitrogen/spinner.gif"}).
-record(image, {?ELEMENT_BASE, image=""}).
-record(rounded_panel, {?ELEMENT_BASE, body="", color=gray}).
-record(lightbox, {?ELEMENT_BASE, body="", opacity=0.8}).
-record(table, {?ELEMENT_BASE, rows}).
-record(tablerow, {?ELEMENT_BASE, cells}).
-record(tableheader, {?ELEMENT_BASE, text="", html_encode=true, body="", align="left", valign="middle", colspan=1, rowspan=1}).
-record(tablecell, {?ELEMENT_BASE, text="", html_encode=true, body="", align="left", valign="middle", colspan=1, rowspan=1}).
-record(singlerow, {?ELEMENT_BASE, cells}). 
-record(file, {?ELEMENT_BASE, file=undefined}).
-record(flash, {?ELEMENT_BASE}).
-record(placeholder, {?ELEMENT_BASE, body=[]}).
-record(bind, {?ELEMENT_BASE, data=[], map=[], transform=undefined, acc=[], body=[]}).

%%% Controls %%%
-define(CONTROL_BASE, ?ELEMENT_BASE, delegate, tag).
-record(inplace_textbox, {?CONTROL_BASE, text="", html_encode=true, start_mode=view, validators=[]}).
-record(wizard, {?CONTROL_BASE, titles=undefined, steps=undefined }).

%%% Actions %%%
-define(ACTION_BASE, trigger, target, actions, show_if=true).
-record(event, {?ACTION_BASE, type=click, interval=1, postback, delegate, controls=[]}).
-record(validate, {?ACTION_BASE, on=submit, success_text=" ", validators, attach_to=undefined }).
-record(validation_error, {?ACTION_BASE, text="" }).
-record(alert, {?ACTION_BASE, text=""}).
-record(confirm, {?ACTION_BASE, text="", postback}).
-record(script, {?ACTION_BASE, script}).
-record(effect_highlight, {?ACTION_BASE, duration=0.8}).
-record(effect_fade, {?ACTION_BASE, duration=0.6}).
-record(effect_appear, {?ACTION_BASE, duration=0.6}).
-record(effect_blindup, {?ACTION_BASE, duration=0.6}).
-record(effect_blinddown, {?ACTION_BASE, duration=0.6}).
-record(effect_pulsate, {?ACTION_BASE, pulses=1, duration=0.3}).
-record(effect_show, {?ACTION_BASE}).
-record(effect_hide, {?ACTION_BASE}).
-record(effect_toggle, {?ACTION_BASE}).
-record(disable_selection, {?ACTION_BASE}).
-record(buttonize, {?ACTION_BASE}).

%%% Validators %%%
-define(VALIDATOR_BASE, text="Failed.").
-record(is_required, {?VALIDATOR_BASE}).
-record(is_email, {?VALIDATOR_BASE}).
-record(is_integer, {?VALIDATOR_BASE}).
-record(min_length, {?VALIDATOR_BASE, length}).
-record(confirm_password, {?VALIDATOR_BASE, password}).
-record(custom, {?VALIDATOR_BASE, function, record }).
-record(js_custom, {?VALIDATOR_BASE, function, args="{}" }).


-endif.