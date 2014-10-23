# Nitrogen TODO

## General functionality

* Look into ErlyDTL integration as alternate templating engine
* Integrated cache server and using cache\_handler

## Elements

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
