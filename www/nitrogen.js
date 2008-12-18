/*** JQUERY SPECIFIC CODE ***/

function wf_observe_event(el, type, func) {
	$(el).bind(type, func);
}

function wf_update(el, html) {
	$(el).html(html);
}

function wf_insert_top(el, html) {
	$(el).prepend(html);
}

function wf_insert_bottom(el, html) {
	$(el).append(html);
}

function wf_draggable(dragObj, dragOptions, dragTag) {
	dragObj.wf_drag_tag = dragTag;
	$(dragObj).draggable(dragOptions);	
}

function wf_droppable(dropObj, dropOptions, dropPostbackInfo) {
	dropOptions.drop = function(ev, ui) {
		var dragItem = ui.draggable[0].wf_drag_tag;
		wf_queue_postback(this.id, dropPostbackInfo, "drag_item=" + dragItem);
	}
	$(dropObj).droppable(dropOptions);
}

function wf_sortitem(sortItem, sortTag) {
	sortItem.wf_sort_tag = sortTag;
}

function wf_sortblock(sortBlock, sortOptions, sortPostbackInfo) {
	sortOptions.update = function() {
		var sortItems = "";
		for (var i=0; i<this.childNodes.length; i++) {
			var n = this.childNodes[i];
			if (sortItems != "") sortItems += ",";
			if (n.wf_sort_tag) sortItems += n.wf_sort_tag;
		}
		wf_queue_postback(this.id, sortPostbackInfo, "sort_items=" + sortItems);
	};
	$(sortBlock).sortable(sortOptions);
}

function wf_serialize_page() {
	var elements = wf_get_elements_to_serialize();
	var s = "";
	for (var i = 0; i<elements.length; i++) {
		s += "&" + $(elements[i]).serialize();
	}
	return s;
}

function wf_get_elements_to_serialize() {
	var tagnames = ["input", "button", "select", "textarea", "checkbox"];
	var a = new Array();
	for (var i=0; i<tagnames.length; i++) {
		var l = document.getElementsByTagName(tagnames[i]);
		for (var j=0; j<l.length; j++) {
			a = a.concat(l[j]);
		}
	}
	
  return a;	
}

function wf_ajax(params) {
	wf_start_spinner();	
	$.ajax({ 
		url: document.location.href,
		type:'post',
		data: params,
		dataType: 'text',
		success: function(data, textStatus) {
			wf_stop_spinner();
			try {
				//alert("SUCCESS: " + transport.responseText);
				eval(data);
			} catch (E) {
				alert("JAVASCRIPT ERROR: " + data);
				alert(E);
			}
			wf_is_in_postback = false;
		},
		error: function(xmlHttpRequest, textStatus, errorThrown) {
			alert("FAIL: " + textStatus);
			wf_is_in_postback = false;
		}
	});			
}

function wf_comet_start(postbackInfo) {
	if (!wf_comet_is_running) {
		wf_comet(postbackInfo);
		wf_comet_is_running = true;
	}
}

function wf_comet(postbackInfo) {
	// Get params...
	var params = 
		"postbackInfo=" + postbackInfo + 
		"&domState=" + wf_dom_state;
	
	$.ajax({ 
		url: document.location.href,
		type:'post',
		data: params,
		dataType: 'text',
		success: function(data, textStatus) {
			try {
				//alert("SUCCESS: " + data);
				eval(data);
			} catch (E) {
				alert("JAVASCRIPT ERROR: " + data);
				alert(E);
			}
			setTimeout("wf_comet('" + postbackInfo + "');", 0);
		},
		error: function(xmlHttpRequest, textStatus, errorThrown) {
			setTimeout("wf_comet('" + postbackInfo + "');", 5000);
		}
	});			
}

/*** POSTBACK LOOP ***/

function wf_postback_loop() {
	setTimeout("wf_postback_loop()", 1);
	if (wf_postbacks.length == 0) return;
	if (wf_is_in_postback) return;
	var o = wf_postbacks.shift();
	wf_do_postback(o.triggerID, o.postbackInfo, o.extraParams);
}

function wf_queue_postback(triggerID, postbackInfo, extraParams) {
	var o = new Object();
	o.triggerID = obj(triggerID).id;
	o.postbackInfo = postbackInfo;
	o.extraParams = extraParams;
	wf_postbacks.push(o);
}

function wf_do_postback(triggerID, postbackInfo, extraParams) {
	// Flag to stop firing multiple postbacks at the same time...
	wf_is_in_postback = true;

	// Check validatation...
	var isValid = true;
	
	var elements = wf_get_elements_to_serialize();
	for (var i=0; i<elements.length; i++) {
		element = elements[i];
		if (element.validator && (element.validator.trigger.id == triggerID) && !element.validator.validate()) {
			isValid = false;
		}
	}
	
	if (!isValid) {
		wf_is_in_postback = false;
		return;
	}
	
	// Get params...
	var params = 
		"postbackInfo=" + postbackInfo + 
		"&domState=" + wf_dom_state + 
		"&" + wf_serialize_page() + 
		"&" + extraParams;
		
	// Go Ajax!
	wf_ajax(params);
}

/*** PATH LOOKUPS ***/


function obj(path) {
	path = wf_normalize_partial_path(path);
	
	// Try the easy option...
	var el = document.getElementById(path);
	if (el) return el;
	
	// Not found, so scan recursively...
	return wf_scan_elements(path, document.childNodes);
}

function wf_scan_elements(path, elements) {
	if (!elements) return;
	
	for (var i=0; i<elements.length; i++) {
		var t = elements[i].id;
		if (t == undefined) continue;
		var pos = t.indexOf(path);
		if (pos == -1) continue;
		if (t.indexOf(path) + path.length == t.length) {
			return elements[i];
		}
	}
	
	for (var i=0; i<elements.length; i++) {
		var el = wf_scan_elements(path, elements[i].childNodes)
		if (el) return el;
	}

	return null;
}

function wf_normalize_partial_path(path) {
	var oldparts = wf_current_path.split(".");
	var newparts = path.split(".");
	var a = new Array();
	for (var i=0; i<newparts.length; i++) {
		var part = newparts[i];
		if (part == "me") a = oldparts;
		else if (part == "parent") a.pop();
		else a.push(part);
	}
	
	return a.join("__");
}

/*** SPINNER ***/

function wf_start_spinner() {
	var spinner = obj('spinner');
	if (spinner) new Effect.Fade(spinner, { duration: 1.0 });
}

function wf_stop_spinner() {
	var spinner = obj('spinner');
	if (spinner) spinner.show();
}


/*** MISC ***/

function wf_return_false(value, args) { 
	return false; 
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

function wf_disable_selection(element) {
    element.onselectstart = function() {
        return false;
    };
    element.unselectable = "on";
    element.style.MozUserSelect = "none";
    element.style.cursor = "default";
}

function wf_set_value(element, value) {
	if (!element.id) element = obj(element);
	if (element.value != undefined) element.value = value;
	else if (element.checked != undefined) element.checked = value;
	else wf_update(element, value);
}

/*** INITIALIZE VARS ***/

var	wf_comet_is_running = false;
var wf_is_in_postback = false;
var wf_dom_state = "";
var wf_postbacks = new Array();
var wf_current_path = "";
wf_postback_loop(); // Start the postback loop.

