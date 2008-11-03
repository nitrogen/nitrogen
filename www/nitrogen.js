function wf_event_loop() {
	setTimeout("wf_event_loop()", 1);
	if (wf_events.length == 0) return;
	if (wf_is_in_event) return;
	var o = wf_events.shift();
	wf_do_event(o.triggerID, o.eventInfo, o.controls);
}

function wf_queue_event(triggerID, eventInfo, controls) {
	var o = new Object();
	o.triggerID = triggerID;
	o.eventInfo = eventInfo;
	o.controls = controls;
	wf_events.push(o);
}

function wf_do_event(triggerID, eventInfo, controls) {
	// Flag to stop firing multiple events at the same time...
	wf_is_in_event = true;

	// Check validatation...
	var elements = $(document.forms[0]).getElements();
	var isValid = true;
	for (var i = 0; i<elements.length; i++) {
		element = elements[i];
		if (element.validator && (element.validator.trigger == triggerID) && !element.validator.validate()) {
			isValid = false;
		}
	}
	if (!isValid) {
		wf_is_in_event = false;
		return;
	}
	
	// Create the dom_paths string...
	var argDomPaths = "";	
	for (var key in dom_paths) {
		if (!dom_paths[key] || dom_paths[key] == "") continue;
		if (argDomPaths != "") argDomPaths += "|";
		argDomPaths += dom_paths[key];
	}
	
	// Get params...
	var params = "eventInfo=" + eventInfo + "&domState=" + wf_dom_state + "&domPaths=" + argDomPaths;
	if (controls && controls.length > 0) {
		for (var i = 0; i<controls.length; i++) {
			var control= obj(controls[i]);
			if (control.serialize) params += "&" + control.serialize();
		}
	} else {
		params += "&" + obj('dom_root.page').serialize();
 	}		 	
	
	wf_start_spinner();
	
	new Ajax.Request(document.location.href, { 
		method:'post',
		parameters: params,
		evalJS: true,
		evalJS: false,
		onSuccess: function(transport) {
			wf_stop_spinner();
			try {
				//alert("SUCCESS: " + transport.responseText);
				eval(transport.responseText);
			} catch (E) {
				alert("JAVASCRIPT ERROR: " + transport.responseText);
				alert(E);
			}
			wf_is_in_event = false;
		},
		onFailure: function(transport) {
			alert("FAIL: " + transport.responseText);
			wf_is_in_event = false;
		}
	});			
}

function wf_is_enter_key(event) {
	return (event && event.keyCode == 13);
}

function wf_go_next(controlID) {
	var o = obj(controlID);
	if (o.focus) o.focus();
	if (o.select) o.select();
	if (o.click) o.click();
}

function wf_link(parentControlID, controlID, htmlControlID, ePath) {
	var s = 
	  controlID + " = new String(\"" + htmlControlID + "\");\n" +
		controlID + ".id= \"" + htmlControlID + "\";\n" +
		controlID + ".isWLinkObject = true;\n" +
		controlID + ".parent = " + parentControlID + ";\n" + 
	  "js_dom_paths[\"" + controlID + "\"] = " + controlID + ";";
	eval(s);
	if (ePath) dom_paths[controlID] = ePath;
}

function wf_start_spinner() {
	var spinner = obj('spinner');
	if (spinner) new Effect.Fade(spinner, { duration: 1.0 });
}

function wf_stop_spinner() {
	var spinner = obj('spinner');
	if (spinner) spinner.show();
}

function wf_return_false(value, args) { 
	return false; 
}

function wf_disable_selection(element) {
    element.onselectstart = function() {
        return false;
    };
    element.unselectable = "on";
    element.style.MozUserSelect = "none";
    element.style.cursor = "default";
}

function wf_set_value(elementID, value) {
	var element = obj(elementID);
	if (element.value != undefined) element.value = value;
	else if (element.checked != undefined) element.checked = value;
	else element.update(value);
}


function obj(t) {
	if (t.isWLinkObject) return $(t.toString());
	if ($(t)) return $(t);
	
	var result = null;
	for (var path in js_dom_paths) {
		if (!path.endsWith(t)) continue;
		if (result != null) alert("Obj() found too many matches for: " + t);
		result = js_dom_paths[path];
	}
	if (result) {
		return $(result.toString());
	} 
	
	return null;
}

var wf_is_in_event = false;
var wf_dom_state = "";
var wf_events = new Array();
var dom_paths = new Object();
var js_dom_paths = new Object();
var dom_root = new Object();
wf_link("dom_root.page", "dom_root.page", "page");
wf_event_loop(); // Start the event loop.