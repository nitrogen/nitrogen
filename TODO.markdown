# Nitrogen TODO

## General functionality

* Look into ErlyDTL integration as alternate templating engine
* Websocket Support, possibly as alternative replacement for comet backends
* Integrated cache server and using cache\_handler
* Elements that have to render text should throw a better error if passed
  nitrogen elements/tuples
* Test suite for `nitrogen_core`, probably using NitrogenProject.com's demos
  section combined with the tool Selenium as the main test, and integrating it
  with javascript testing. This will ensure many of the elements succeed in
  rendering without crashing.

## Elements

* Add `#progress_bar` element
* `#upload` element needs to use `#progress_bar` element.
* Add Code-block element
* Add Mathjax element = http://mathjax.org
* Add wf\_render\_elements:map\_body/2, to recursively apply a function to
  child elements

## Actions and Validators

* Make wf:wire should be able to wire validations without having to wrap those
  validations in a #validation action. Basically, wf:wire can determine if the
  specified action is a validator and automatically wrap it.
* Look into a lightweight Nitrogen-specific replacement for LiveValidation. LV
  just a little too obtuse.

## Documentation

[Documentation has its own TODO](https://github.com/nitrogen/nitrogen_core/blob/master/doc/org-mode/README.markdown)
