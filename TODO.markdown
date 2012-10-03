# Nitrogen TODO

## General functionality

* Look into ErlyDTL integration as alternate templating engine
* Websocket Support, possibly as alternative replacement for comet backends
* Email integration with gen\_smtp and its own handler (mail\_handler?)
* Integrated cache server and using cache\_handler

## Installation/Platform

* Take advantage of the new functionality in R15B02 to make packages/releases that don't include a full ERTS installation, but instead rely on the installed copy of ERTS.

## Elements

* Add Continuous Scrolling Element for loading dynamic content
* Add a "Pagination" element for use in applications that need to show a list of many items paginated (ie a list of user accounts, or transactions), with it's own set of postbacks
* Add Code-block element
* Add Mathjax element = http://mathjax.org
* Add 'type' to textbox elements to support HTML5 alternate types (number, url, email, etc)
* Add wf_render_elements:map_body/2, to recursively apply a function to child elements
* Add support for element_X:transform_element for elements which 

## Actions and validators

* Validators modified like Zotonic so they don't crash when fields are removed

## Documentation

[Documentation has its own TODO](http://github.com/nitrogen/nitrogen_core/docs/org-mode/README.markdown)
