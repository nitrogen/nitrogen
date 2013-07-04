# Nitrogen TODO

## General functionality

* Add error handler behaviour for defining how to handle crashes, instead of either printing "Internal Server Error", or in the case of ajax/comet, simply silently failing.
* Look into ErlyDTL integration as alternate templating engine
* Websocket Support, possibly as alternative replacement for comet backends
* Email integration with gen\_smtp and its own handler (mail\_handler?)
* Integrated cache server and using cache\_handler
* Elements that have to render text should throw a better error if passed nitrogen elements/tuples

## Installation/Platform

* Provide a smoother mechanism for including Nitrogen in an application as a simple rebar dependency

## Elements

* Add Code-block element
* Add Mathjax element = http://mathjax.org
* Add 'type' to textbox elements to support HTML5 alternate types (number, url, email, etc)
* Add wf\_render\_elements:map\_body/2, to recursively apply a function to child elements
* Add progress bar element

## Actions and Validators

* Make wf:wire should be able to wire validations without having to wrap those validations in a #validation action. Basically, wf:wire can determine if the specified action is a validator and automatically wrap it.
* Add an action to ensure a javascript file is loaded. #ensure\_loaded action along with an associated wf:ensure\_loaded/1 function.
* Make a general #confirm\_general validator that works like the #confirm\_password validator. Then change the #confirm\_password validator to use this new validator. This is for things like "confirm your email address".  The current method of just using #confirm\_password *works* already, but is not intuitivel and the field confirmation should be synchronized.
* Due to the commonality of using the #confirm\_password validator on password fields, add an attribute to the #password called `confirm`, which would take the ID of the confirmation textbox/password element. Possibly add this to the #textbox element as well.

## Documentation

[Documentation has its own TODO](https://github.com/nitrogen/nitrogen_core/blob/master/doc/org-mode/README.markdown)
