# Nitrogen TODO

## General functionality

* Look into ErlyDTL integration as alternate templating engine
* Websocket Support, possibly as alternative replacement for comet backends
* Email integration with gen\_smtp and its own handler (mail\_handler?)
* Integrated cache server and using cache\_handler
* Elements that have to render text should throw a better error if passed nitrogen elements/tuples

## Installation/Platform

* Take advantage of the new functionality in R15B02 to make packages/releases that don't include a full ERTS installation, but instead rely on the installed copy of ERTS.

## Elements

* Elements can redefined in terms of other Nitrogen elements, and doing so shoul not invoke the overhead associate with typical rendering. So in wf\_render\_elements, we can add a check for ElementMod:transform\_element/1, which basically calls it without.  Before doing this, the speed of such a call should be tested to determine if this is something that should be cached.
* Add Continuous Scrolling Element for loading dynamic content
* Add a "Pagination" element for use in applications that need to show a list of many items paginated (ie a list of user accounts, or transactions), with it's own set of postbacks
* Add Code-block element
* Add Mathjax element = http://mathjax.org
* Add 'type' to textbox elements to support HTML5 alternate types (number, url, email, etc)
* Add wf\_render\_elements:map\_body/2, to recursively apply a function to child elements
* Add support for element\_X:transform\_element for elements which 
* Add progress bar element

## Actions and Validators

* Validators modified like Zotonic so they don't crash when fields are removed
* Make wf:wire should be able to wire validations without having to wrap those validations in a #validation action. Basically, wf:wire can determine if the specified action is a validator and automatically wrap it.

## Documentation

[Documentation has its own TODO](https://github.com/nitrogen/nitrogen_core/blob/master/doc/org-mode/README.markdown)
