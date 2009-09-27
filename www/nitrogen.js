// The idea is to have one high level
// Nitrogen object, created from NitrogenClass, that 
// encapsulates everything in order to prevent collisions.

function NitrogenClass(o) {
	this.$url = document.location.href;
	this.$div = document;
	this.$params = new Object();
	this.$current_path = "";
	this.$event_queue = new Array();
	this.$event_is_running = false;
	return this;
}

/*** PRIVATE METHODS ***/

NitrogenClass.prototype.$scope = function(path) {
	this.$current_path = path;
}

NitrogenClass.prototype.$set_param = function(key, value) {
	this.$params[key] = value;
}

/*** EVENT QUEUE ***/

NitrogenClass.prototype.$queue_event = function(triggerID, eventContext, extraParams) {
	// Put an event on the event_queue.
	this.$event_queue.push({
		triggerID    : this.obj(triggerID).id,
		eventContext : eventContext,
		extraParams  : extraParams
	});
}


NitrogenClass.prototype.$event_loop = function() {
	// Make it loop.
	setTimeout("Nitrogen.$event_loop();", 1);
	
	// If something is running, or the queue is empty, then just return.
	if (this.$event_is_running) return;
	if (this.$event_queue.length == 0) return;
	
	// Get and exect the event.
	var o = this.$event_queue.shift();
	this.$do_event(o.triggerID, o.eventContext, o.extraParams);
}

/*** VALIDATE AND SERIALIZE ***/

NitrogenClass.prototype.$validate_and_serialize = function(triggerID) {
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
			}
			params.push(jQuery(element).serialize());
		}
	}
		
	// Return the params if valid. Otherwise, return null.
	if (is_valid) {
		return params.join("&");
	} else {
		return null;
	}
}

NitrogenClass.prototype.$get_dom_paths = function() {
	// Build list of paths...
	var paths=new Array();
	var elements = this.$div.getElementsByTagName('*');
	for (var i=0; i<elements.length; i++) {
		var element=elements[i];
		if (element.id) {
			paths.push(elements[i].id);
		}
	}
	
	return paths.join(",");
}

/*** AJAX METHODS ***/

NitrogenClass.prototype.$do_event = function(triggerID, eventContext, extraParams) {
	// Flag to prevent firing multiple postbacks at the same time...
	this.$event_is_running = true;

	if (!extraParams) extraParams="";

	// Run validation...
	var s = this.$validate_and_serialize(triggerID);	
	if (s == null) {
		this.$event_is_running = false;
		return;
	}
	
	// Assemble other parameters... 
	var params = "";
	params += "eventContext=" + eventContext + "&";
	params += "domPaths=" + this.$get_dom_paths() + "&";
	params += extraParams + "&";
	params += s + "&";
	for (var key in this.$params) {
		params += key + "=" + this.$params[key] + "&";
	}
	
	var n = this;

	jQuery.ajax({ 
		url: this.$url,
		type:'post',
		data: params,
		dataType: 'text',
		success: function(data, textStatus) {
			n.$event_is_running = false;
			eval(data);
		},
		error: function(xmlHttpRequest, textStatus, errorThrown) {
			n.$event_is_running = false;
		}
	});			
}

/*** SYSTEM EVENTS (FOR ASYNC) ***/

NitrogenClass.prototype.$do_system_event = function(eventContext) { 
	// Assemble parameters... 
	var params = "";
	params += "eventContext=" + eventContext + "&";
	params += "domPaths=" + this.$get_dom_paths() + "&";
	for (var key in this.$params) {
		params += key + "=" + this.$params[key] + "&";
	}

	var n = this;

	$.ajax({ 
		url: this.$url,
		type:'post',
		data: params,
		dataType: 'text',
		success: function(data, textStatus) {
			eval(data);
		},
		error: function(xmlHttpRequest, textStatus, errorThrown) {
		}
	});                     
}

/*** FILE UPLOAD ***/
NitrogenClass.prototype.$upload = function(form) {
	// Assemble other parameters...
	form.action = this.$url;
	form.domPaths.value = this.$get_dom_paths();
	form.pageContext.value = this.$params["pageContext"];
	form.submit();
	form.reset();
	form.action = "";
}

/*** SERIALIZATION ***/

NitrogenClass.prototype.$get_form_elements = function() {
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

NitrogenClass.prototype.obj = function(path) {
	// Clean the path...
	path = this.$normalize_partial_path(path);

	// Special case for the 'page' element, which is the entire document.
	if (path == "page") return document;	
	
	// Try the easy option...
	var el = document.getElementById(path);
	if (el) return el;
	
	// Not found, so scan recursively...
	return this.$scan_elements(path, this.$div.childNodes);
}

NitrogenClass.prototype.$normalize_partial_path = function(path) {
	var oldparts = this.$current_path.split(".");
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

NitrogenClass.prototype.$scan_elements = function(path, elements) {
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
		var el = this.$scan_elements(path, elements[i].childNodes)
		if (el) return el;
	}

	return null;
}


/*** EVENT WIRING ***/

NitrogenClass.prototype.$observe_event = function(el, type, func) {
	jQuery(el).bind(type, func);
}



/*** DYNAMIC UPDATING ***/

NitrogenClass.prototype.$update = function(el, html) {
	jQuery(el).html(html);
}

NitrogenClass.prototype.$insert_top = function(el, html) {
	jQuery(el).prepend(html);
}

NitrogenClass.prototype.$insert_bottom = function(el, html) {
	jQuery(el).append(html);
}


/*** MISC ***/

NitrogenClass.prototype.$return_false = function(value, args) { 
	return false; 
}

NitrogenClass.prototype.$is_enter_key = function(event) {
	return (event && event.keyCode == 13);
}

NitrogenClass.prototype.$go_next = function(controlID) {
	var o = this.obj(controlID);
	if (o.focus) o.focus();
	if (o.select) o.select();
	if (o.click) o.click();
}

NitrogenClass.prototype.$disable_selection = function(element) {
	element.onselectstart = function() {
	    return false;
	};
	element.unselectable = "on";
	element.style.MozUserSelect = "none";
	element.style.cursor = "default";
}

NitrogenClass.prototype.$set_value = function(element, value) {
	if (!element.id) element = obj(element);
	if (element.value != undefined) element.value = value;
	else if (element.checked != undefined) element.checked = value;
	else this.$update(element, value);
}

NitrogenClass.prototype.$normalize_param = function(key, value) {
	// Create the key=value line to add.
	// Sometimes, the user will pass a bunch of params in the key field.
	var s = "";
	if (key) { s = key; }
	if (key && value) { s = key + "=" + value; }
	return key + "&" + value;
}

/*** DATE PICKER ***/

NitrogenClass.prototype.$datepicker = function(pickerObj, pickerOptions) {
	jQuery(pickerObj).datepicker(pickerOptions);
}


/*** DRAG AND DROP ***/

NitrogenClass.prototype.$draggable = function(dragObj, dragOptions, dragTag) {
	dragObj.$drag_tag = dragTag;
	jQuery(dragObj).draggable(dragOptions);	
}

NitrogenClass.prototype.$droppable = function(dropObj, dropOptions, dropPostbackInfo) {
	var n = this;
	dropOptions.drop = function(ev, ui) {
		var dragItem = ui.draggable[0].$drag_tag;
		n.$queue_event(this.id, dropPostbackInfo, "drag_item=" + dragItem);
	}
	jQuery(dropObj).droppable(dropOptions);
}



/*** SORTING ***/

NitrogenClass.prototype.$sortitem = function(sortItem, sortTag) {
	sortItem.$sort_tag = sortTag;
	sortItem.$drag_tag = sortTag;
}

NitrogenClass.prototype.$sortblock = function(sortBlock, sortOptions, sortPostbackInfo) {
	var n = this;
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


function obj(path) {
	return Nitrogen.obj(path);
}

var Nitrogen = new NitrogenClass();
Nitrogen.$event_loop();