/*
Usage:
	var n = new Nitrogen({
		url : "http://nitrogenserver/web/module",
		div : enclosingDiv
	});
	
	n.IFrame(div, url);
	n.Inline(div, url);
	n.Windex(div, url);

*/

function Nitrogen(o) {
	// Set the id, and associate with the global Nitrogen object...
	if (o.id) {
		this.id = o.id
	} else {
		this.id = "o" + Math.floor(Math.random()*999999999);
	}
	eval(Nitrogen.$NString + "." + this.id + " = this;");

	// Set the originating URL...
	if (o.url) {
		this.$url = o.url;
	} else {
		this.$url = document.location.href;
	}

	// Set some initial properties.
	if (o.div) {
		this.$div = o.div;
	} else {
		this.$div = document;
	}
	
	// Clear the dom_state...
	this.$params = new Object();
	this.$dom_state = "";
	this.$comet_is_running = false;
}

var N = Nitrogen;
N.$NString = "Nitrogen";
N.$current_id = "";
N.$current_path = "";
N.$event_queue = new Array();
N.$event_is_running = false;

/*** PUBLIC METHODS ***/

function obj(path) {
	return Nitrogen.obj(path);
}

N.Page = function(o) {
	var n = new Nitrogen(o);
	n.$do_event = n.$do_xhr_event;
	n.$do_comet = n.$do_xhr_comet;
	return n;
}

N.Inline = function(o) {
	var n = new Nitrogen(o);
	if (o.windex) {
		n.$do_event = n.$do_windex_event;
		n.$do_comet = n.$do_windex_comet;
		n.$url = Nitrogen.$add_param_to_url(n.$url, "windex", "true");
	} else {
		n.$do_event = n.$do_xhr_event;
		n.$do_comet = n.$do_xhr_comet;
	}
	
	var url = Nitrogen.$add_param_to_url(n.$url, "object_id", n.id);
	Nitrogen.$load_script(url);
	return n;
}


/*** PRIVATE METHODS ***/

N.$scope = function(id, path) {
	N.$current_id = id;
	N.$current_path = path;
}

N.$lookup = function(id) {
	return eval(Nitrogen.$NString + "." + id + ";");
}

N.$set_dom_state = function(s) {
	var n = Nitrogen.$lookup(Nitrogen.$current_id);
	n.$set_dom_state(s);
}

N.prototype.$set_dom_state = function(s) {
	this.$dom_state = s;
}

N.$set_param = function(key, value) {
	var n = Nitrogen.$lookup(Nitrogen.$current_id);
	n.$set_param(key, value);	
}

N.prototype.$set_param = function(key, value) {
	this.$params[key] = value;
}

/*** EVENT QUEUE ***/

N.$queue_event = function(triggerID, eventContext, extraParams) {
	var n = Nitrogen.$lookup(Nitrogen.$current_id);
	n.$queue_event(triggerID, eventContext, extraParams);
}

N.prototype.$queue_event = function(triggerID, eventContext, extraParams) {
	// Put an event on the event_queue.
	Nitrogen.$event_queue.push({
		n : this,
		triggerID    : this.obj(triggerID).id,
		eventContext : eventContext,
		extraParams  : extraParams
	});
}


N.$event_loop = function() {
	// Make it loop.
	setTimeout(Nitrogen.$NString + ".$event_loop();", 1);
	
	// If something is running, or the queue is empty, then just return.
	if (Nitrogen.$event_is_running) return;
	if (Nitrogen.$event_queue.length == 0) return;
	
	// Get and exect the event.
	var o = Nitrogen.$event_queue.shift();
	o.n.$do_event(o.triggerID, o.eventContext, o.extraParams);
}

/*** VALIDATE AND SERIALIZE ***/

N.prototype.$validate_and_serialize = function(triggerID) {
	// Check validatation, build list of params...
	var is_valid = true;
	var elements = this.$get_form_elements();
	var params=new Array();
	for (var i=0; i<elements.length; i++) {
		var element = elements[i];
		if (element.validator && (element.validator.trigger.id == triggerID) && !element.validator.validate()) {
			// Don't short circuit here, because we want to update all of the validator UI.
			is_valid = false;
		} else {
			if (element.type == "radio") {
				params.push(element.id + "=" + element.checked);
			} else {
				params.push(jQuery(element).serialize());
			}
		}
	}
	
	// Build list of paths...
	var paths=new Array();
	var elements = this.$div.getElementsByTagName('*');
	for (var i=0; i<elements.length; i++) {
		var element=elements[i];
		if (element.id) {
			paths.push(elements[i].id);
		}
	}
	
	// Return the params if valid. Otherwise, return null.
	if (is_valid) {
		return params.join("&") + "&domPaths=" + paths.join(",");
	} else {
		return null;
	}
}

/*** AJAX METHODS ***/

N.prototype.$do_xhr_event = function(triggerID, eventContext, extraParams) {
	// Flag to prevent firing multiple postbacks at the same time...
	Nitrogen.$event_is_running = true;

	if (!extraParams) extraParams="";

	// Run validation...
	var s = this.$validate_and_serialize(triggerID);	
	if (s == null) {
		Nitrogen.$event_is_running = false;
		return;
	}
	
	// Assemble other parameters... 
	var url = this.$url;
	for (var key in this.$params) {
		url = Nitrogen.$add_param_to_url(url, key, this.$params[key]);
	}

	// Build params...
	var params = "eventContext=" + eventContext  + "&" + s + "&" + extraParams;
	
	jQuery.ajax({ 
		url: url,
		type:'post',
		data: params,
		dataType: 'text',
		success: function(data, textStatus) {
			Nitrogen.$event_is_running = false;
			eval(data);
		},
		error: function(xmlHttpRequest, textStatus, errorThrown) {
			Nitrogen.$event_is_running = false;
		}
	});			
}

N.$comet_start = function(eventContext) {
	var n = Nitrogen.$lookup(Nitrogen.$current_id);
	n.$comet_start(eventContext);
}

N.prototype.$comet_start = function(eventContext) {
	this.$do_comet(eventContext);
}

N.prototype.$do_xhr_comet = function(eventContext) { 
	if (this.$comet_is_running) return;
	this.$comet_is_running = true;

	// Get params...
  var params = 
	  "eventContext=" + eventContext + 
	  "&domState=" + this.$dom_state;
	
	var n = this;

	$.ajax({ 
		url: this.$url,
		type:'post',
		data: params,
		dataType: 'text',
		success: function(data, textStatus) {
			eval(data);
			n.$comet_is_running = false;
			setTimeout("Nitrogen." + n.id + ".$comet_start('" + eventContext + "');", 0);
		},
		error: function(xmlHttpRequest, textStatus, errorThrown) {
			n.$comet_is_running = false;
			setTimeout("Nitrogen." + n.id + ".$comet_start('" + eventContext + "');", 5000);
		}
	});                     
}



/*** WINDEX METHODS ***/

N.prototype.$do_windex_event = function(triggerID, eventContext, extraParams) { 
	// Run validation...
	var s = this.$validate_and_serialize(triggerID);	
	if (s == null) {
		return;
	}
	
	// Build params...
	var url = this.$url;
	url = Nitrogen.$add_param_to_url(url, "domState", this.$dom_state);
	url = Nitrogen.$add_param_to_url(url, "eventContext", eventContext);
	url = Nitrogen.$add_param_to_url(url, s);
	url = Nitrogen.$add_param_to_url(url, extraParams);
	Nitrogen.$load_script(url);
}


N.prototype.$do_windex_comet = function(eventContext) { 
	alert("Comet is not yet supported via Windex.");
}

/*** FILE UPLOAD ***/
N.$upload = function(form) {
	var n = Nitrogen.$lookup(Nitrogen.$current_id);
	form.domState.value = n.$dom_state;
	form.action = n.$url;
	form.submit();
	form.reset();
}

/*** SERIALIZATION ***/

N.prototype.$get_form_elements = function() {
	var tagnames = ["input", "button", "select", "textarea", "checkbox"];
	var a = new Array();
	for (var i=0; i<tagnames.length; i++) {
		var l = this.$div.getElementsByTagName(tagnames[i]);
		for (var j=0; j<l.length; j++) {
			a = a.concat(l[j]);
		}
	}
	
  return a;	
}

/*** PATH LOOKUPS ***/

N.obj = function(path) {
	var n = Nitrogen.$lookup(Nitrogen.$current_id);
	return n.obj(path);
}

N.prototype.obj = function(path) {
	// Clean the path...
	path = N.$normalize_partial_path(path);

	// Special case for the 'page' element, which is the entire document.
	if (path == "page") return document;	
	
	// Try the easy option...
	var el = document.getElementById(path);
	if (el) return el;
	
	// Not found, so scan recursively...
	return Nitrogen.$scan_elements(path, this.$div.childNodes);
}

N.$normalize_partial_path = function(path) {
	var oldparts = Nitrogen.$current_path.split(".");
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

N.$scan_elements = function(path, elements) {
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
		var el = Nitrogen.$scan_elements(path, elements[i].childNodes)
		if (el) return el;
	}

	return null;
}


/*** EVENT WIRING ***/

N.$observe_event = function(el, type, func) {
	jQuery(el).bind(type, func);
}



/*** DYNAMIC UPDATING ***/

N.$update = function(el, html) {
	jQuery(el).html(html);
}

N.prototype.$update = function(html) {
	jQuery(this.$div).html(html);
}

N.$insert_top = function(el, html) {
	jQuery(el).prepend(html);
}

N.$insert_bottom = function(el, html) {
	jQuery(el).append(html);
}


/*** MISC ***/

N.$return_false = function(value, args) { 
	return false; 
}

N.$is_enter_key = function(event) {
	return (event && event.keyCode == 13);
}

N.$go_next = function(controlID) {
	var o = Nitrogen.obj(controlID);
	if (o.focus) o.focus();
	if (o.select) o.select();
	if (o.click) o.click();
}

N.$disable_selection = function(element) {
	element.onselectstart = function() {
	    return false;
	};
	element.unselectable = "on";
	element.style.MozUserSelect = "none";
	element.style.cursor = "default";
}

N.$set_value = function(element, value) {
	if (!element.id) element = obj(element);
	if (element.value != undefined) element.value = value;
	else if (element.checked != undefined) element.checked = value;
	else this.$update(element, value);
}

N.$add_param_to_url = function(url, key, value) {
	// Create the key=value line to add.
	// Sometimes, the user will pass a bunch of params in the key field.
	var s = "";
	if (key) { s = key; }
	if (key && value) { s = key + "=" + value; }
	
	// Return the updated url...
	var parts = url.split("?");
	if (parts.length == 1) { return url + "?" + s; }
	if (parts.length > 1) { return url + "&" + s; }
}

N.$load_script = function(url) {
	var head = document.getElementsByTagName('head')[0];
  var script = document.createElement('script');
  script.type= 'text/javascript';
  script.src= url;
  head.appendChild(script);
}

/*** DATE PICKER ***/

N.$datepicker = function(pickerObj, pickerOptions) {
	jQuery(pickerObj).datepicker(pickerOptions);
}


/*** DRAG AND DROP ***/

N.$draggable = function(dragObj, dragOptions, dragTag) {
	dragObj.$drag_tag = dragTag;
	jQuery(dragObj).draggable(dragOptions);	
}

N.$droppable = function(dropObj, dropOptions, dropPostbackInfo) {
	var n = Nitrogen.$lookup(Nitrogen.$current_id);
	dropOptions.drop = function(ev, ui) {
		var dragItem = ui.draggable[0].$drag_tag;
		n.$queue_event(this.id, dropPostbackInfo, "drag_item=" + dragItem);
	}
	jQuery(dropObj).droppable(dropOptions);
}



/*** SORTING ***/

N.$sortitem = function(sortItem, sortTag) {
	sortItem.$sort_tag = sortTag;
	sortItem.$drag_tag = sortTag;
}

N.$sortblock = function(sortBlock, sortOptions, sortPostbackInfo) {
	var n = Nitrogen.$lookup(Nitrogen.$current_id);
	sortOptions.update = function() {
		var sortItems = "";
		for (var i=0; i<this.childNodes.length; i++) {
			var childNode = this.childNodes[i];
			if (sortItems != "") sortItems += ",";
			if (childNode.$sort_tag) sortItems += childNode.$sort_tag;
		}
		n.$queue_event(this.id, sortPostbackInfo, "sort_items=" + sortItems);
	};
	jQuery(sortBlock).sortable(sortOptions);
}


Nitrogen.$event_loop();
Nitrogen.Page({ id : 'page' });
