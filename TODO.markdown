# Nitrogen TODO

## General functionality

* Look into ErlyDTL integration as alternate templating engine
* Websocket Support, possibly as alternative replacement for comet backends
* Email integration with gen\_smtp and its own handler (mail\_handler?)
* Integrated cache server and using cache\_handler
* Elements that have to render text should throw a better error if passed nitrogen elements/tuples

## Installation/Platform

* Take advantage of the new functionality in R15B02 to make packages/releases that don't include a full ERTS installation, but instead rely on the installed copy of ERTS. 
* Provide a smoother mechanism for including Nitrogen in an application as a simple rebar dependency
* Provide a simple mechanism for including custom elements that might include custom javascript files (this would go along with the #ensure\_loaded{} action below). Possible solution is to add a default "addons" directory inside the static directory (as well as providing default routing maps for the servers that would need it). We would then incorporate the use of a rebar post hook that would scan all the included addons for the existance of a "js" directory, which would be copied (on windows) or symlinked (on linux platforms) into the static/addon directory to be included in the application. So, for example, for a custom Nitrogen element "foo", we would have a directory called "js" within the "foo" application, we could add it as a rebar dependency, and upon building the app and downloading the dependency, there would be a static/addons/foo/foo.js file.  To simplify the process of including these addons, we would incorporate into the standard includes an "addons.hrl", which would be dynamically built each time we use 'make' on the project, and would include links to all the include files in each nitrogen element dependency. That way, we don't have to add a million -include() directives into each page we wish to use our files

## Elements

* Elements can be redefined in terms of other Nitrogen elements, and doing so should not invoke the overhead associate with typical rendering. So in wf\_render\_elements, we can add a check for ElementMod:transform\_element/1, which basically calls it without.  Before doing this, the speed of such a call should be tested to determine if this is something that should be cached.
* Add Continuous Scrolling Element for loading dynamic content
* Add a "Pagination" element for use in applications that need to show a list of many items paginated (ie a list of user accounts, or transactions), with it's own set of postback
* Add Code-block element
* Add Mathjax element = http://mathjax.org
* Add 'type' to textbox elements to support HTML5 alternate types (number, url, email, etc)
* Add wf\_render\_elements:map\_body/2, to recursively apply a function to child elements
* Add support for element\_X:transform\_element for elements which 
* Add progress bar element

## Actions and Validators

* Make wf:wire should be able to wire validations without having to wrap those validations in a #validation action. Basically, wf:wire can determine if the specified action is a validator and automatically wrap it.
* Add an action to ensure a javascript file is loaded. #ensure\_loaded action along with an associated wf:ensure\_loaded/1 function.
* Make a general #confirm\_general validator that works like the #confirm\_password validator. Then change the #confirm\_password validator to use this new validator. This is for things like "confirm your email address".  The current method of just using #confirm\_password *works* already, but is not intuitivel and the field confirmation should be synchronized.
* Due to the commonality of using the #confirm\_password validator on password fields, add an attribute to the #password called `confirm`, which would take the ID of the confirmation textbox/password element. Possibly add this to the #textbox element as well.

## Documentation

[Documentation has its own TODO](https://github.com/nitrogen/nitrogen_core/blob/master/doc/org-mode/README.markdown)
