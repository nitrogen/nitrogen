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
	this.$system_event_queue = new Array();
	this.$system_event_is_running = false;
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

NitrogenClass.prototype.$queue_event = function(triggerID, eventContext, extraParam) {
	// Put an event on the event_queue.
	this.$event_queue.push({
		triggerID    : this.obj(triggerID).id,
		eventContext : eventContext,
		extraParam   : extraParam
	});
}

NitrogenClass.prototype.$queue_system_event = function(eventContext) {
	// Put an event on the event_queue.
	this.$system_event_queue.push({
		eventContext : eventContext
	});
}

NitrogenClass.prototype.$event_loop = function() {
	// Make it loop.
	setTimeout("Nitrogen.$event_loop();", 1);

	// If no events are running and an event is queued, then fire it.
	if (!this.$system_event_is_running && this.$system_event_queue.length > 0) {
    	var o = this.$system_event_queue.shift();
    	this.$do_system_event(o.eventContext);
	}
	
	// If no events are running and an event is queued, then fire it.
	if (!this.$event_is_running && this.$event_queue.length > 0) {
    	var o = this.$event_queue.shift();
    	this.$do_event(o.triggerID, o.eventContext, o.extraParam);
	}
}

/*** VALIDATE AND SERIALIZE ***/

NitrogenClass.prototype.$validate_and_serialize = function(triggerID) {
	// Check validatation, build list of params...
	var is_valid = true;
	var elements = jQuery(":input, :text, :password, :radio, :checkbox, :submit, :image, :reset, :button").not(".no_postback").get();
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
	paths = paths.sort();
	var condensedPaths = this.$condense_dom_paths(paths);
	var combinedPaths = this.$combine_dom_paths(condensedPaths);
	return combinedPaths;
}

NitrogenClass.prototype.$condense_dom_paths = function(paths) {
	var a = new Object();
	for (var i=0; i<paths.length; i++) {
		var parts=paths[i].split("__");
		var b = a;
		for (var j=0; j<parts.length; j++) {
			if (!b[parts[j]]) b[parts[j]] = new Object();
			b = b[parts[j]];
		}
	}
	return a;
}

NitrogenClass.prototype.$combine_dom_paths = function(paths) {
	var s = "";
	for (var key in paths) {
		if (s != "") s += ",";
		s += key;
		var inner = NitrogenClass.prototype.$combine_dom_paths(paths[key]);
		if (inner != "") s += "(" + inner + ")";
	}
	return s;
}


/*** AJAX METHODS ***/

NitrogenClass.prototype.$do_event = function(triggerID, eventContext, extraParam) {
	// Flag to prevent firing multiple postbacks at the same time...
	this.$event_is_running = true;

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
	params += extraParam + "&";
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
	// Flag to prevent firing multiple postbacks at the same time...
	this.$system_event_is_running = true;

	// Assemble parameters... 
	var params = "";
	params += "eventContext=" + eventContext + "&";
	params += "domPaths=" + this.$get_dom_paths() + "&";
	for (var key in this.$params) {
		params += key + "=" + this.$params[key] + "&";
	}
	params += "is_system_event=1"

	var n = this;

	$.ajax({ 
		url: this.$url,
		type:'post',
		data: params,
		dataType: 'text',
		success: function(data, textStatus) {
			n.$system_event_is_running = false;
			// A system event shouldn't clobber the pageContext.
			// Easiest to cacount for it here.
            var pc = n.$params["pageContext"];
			eval(data);
            n.$set_param("pageContext", pc);
		},
		error: function(xmlHttpRequest, textStatus, errorThrown) {
			n.$system_event_is_running = false;
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
		var pos = t.lastIndexOf(path);
		if (pos == -1) continue;
		if (pos + path.length == t.length) {
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

NitrogenClass.prototype.$replace = function(el, html) {
	jQuery(el).replaceWith(html);
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

NitrogenClass.prototype.$is_key_code = function(event, keyCode) {
	return (event && event.keyCode == keyCode);
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

NitrogenClass.prototype.$encode_arguments_object = function(Obj) {
	if (! Bert) { alert("Bert.js library not included in template.") }
	var a = new Array();
	for (var i=0; i<Obj.length; i++) {
		a.push(Obj[i]);
	}
	var s = Bert.encode(a);
	return "args=" + this.$urlencode(s);
}

NitrogenClass.prototype.$urlencode = function(str) {
	return escape(str).replace(/\+/g,'%2B').replace(/%20/g, '+').replace(/\*/g, '%2A').replace(/\//g, '%2F').replace(/@/g, '%40');
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
var page = document;
Nitrogen.$event_loop();