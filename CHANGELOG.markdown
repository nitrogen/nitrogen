# Nitrogen 2.x

## Nitrogen 2.1.0

* Split dependent projects into separate .git repos, including nitrogen_core, nprocreg, and simple_bridge
* Move Nitrogen project (plus sub-apps) to github.com/nitrogen
* Remove ./apps directory.
* Update sample project (built using 'make rel_*') to start up using a first-class Erlang application.
* Add new sync project dependency, allowing for automatic code reloading.

## Nitrogen 2.0.4

* Implement wf:flash(FlashID, Elements) so that flash notice can
  be manipulated/closed/etc.
* Add support for running Nitrogen underneath Webmachine. This makes
  it easier to run Nitrogen and Webmachine side-by-side, and allows
  the use of Webmachine dispatch table to route requests to Nitrogen.
* Add ability to generate Nitrogen-Webmachine packages.
* Update Mochiweb dependency to 1.5.0.
* Update Yaws dependency to 1.89.

## Nitrogen 2.0.3

* Implement callbacks to chain Ajax effects (Jonas Ådahl)
* Fix element name normalization (Jonas Ådahl)
* Support for HTML5 semantic elements (Rajiv M Ranganath)
* Fix recv_from_socket bug when running on Yaws (Gregory Haskins)
* Update livevalidation.js dependency, fixes validation bug (Boris 'billiob' Faure)
* Fix bug causing postbacks to continually grow in certain cases (Jonas Ådahl)
* Added rudimentary support for building and running on Windows (Rusty Klophaus)
* Allow creating page, element, and action stubs from Erlang console more easily (Rusty Klophaus)

## Nitrogen 2.0.2

* Fixed mochiweb bug that caused other headers to get clobbered when using default content type.
* Fixed typo in nprocreg causing sessions to disappear.
* Added #inplace_textarea element
* Added #textbox_autocomplete element
* Refactored nitrogen.js to be more JQuery-ish
* Fixed bug preventing flash messages from displaying in some cases.
* Added #link.title field.
* Restored missing wf:logout() function.
* Fixed max length validator.
* Throw an error if a template file is not found.

## Nitrogen 2.0.1

* Removed mnesia-based process registry, create new distributed process registry. (./apps/nprocreg)
* Fixed Yaws and Mochiweb SimpleBridge to set a default Content-Type. [#60] [#69]
* Fixed Inets SimpleBridge to properly set headers specified as lists rather than atoms. [#60] [#70]
* Disabled Ajax Caching. [#45]
* Ensure site/actions and site/elements directories exist. [#49] [#75]
* Fix page prototype used by "bin/dev page <page>" [#50]
* Update #container_12{} and #container_16{} to use supplied class names and ids [#51]
* Added wf:socket() command [#56]
* Made checkbox readable by wf:q/1 [#58]
* No longer encoding space as &nbsp in html_encode method. [#77]
* Fixed comet timeout value. [#74]
* Updated comet code to prevent spawned process leakage in certain cases.
* Update http_basic_auth_security_handler to protect selective modules. [#76]
* Properly return request_body in Mochiweb SimpleBridge response [#73]
* Fixed "Object Expected" error in IE8 [#57]

## Nitrogen 2.0.0 - Big Release/New Features

### New Elements, Actions, and API functions

* wf:wire can now act upon CSS classes or full JQuery Paths, not just Nitrogen elements. For example, wf:wire(".people > .address", Actions) will wire actions to any HTML elements with an "address" class underneath a "people" class. Anything on http://api.jquery.com/category/selectors/ is supported
* Added wf:replace(ID, Elements), remove an element from the page, put new elements in their place.
* Added wf:remove(ID), remove an element from the page.
* New #api{} action allows you to define a javascript method that takes parameters and will trigger a postback. Javascript parameters are automatically translated to Erlang, allowing for pattern matching. 
* New #grid{} element provides a Nitrogen interface to the 960 Grid System (http://960.gs) for page layouts.
* The #upload{} event callbacks have changed. Event fires both on start of upload and when upload finishes. 
* Upload callbacks take a Node parameter so that file uploads work even when a postback hits a different node.
* Many methods that used to be in 'nitrogen.erl' are now in 'wf.erl'. Also, some method signatures in wf.erl have changed.
* wf:get_page_module changed to wf:page_module
* wf:q(ID) no longer returns a list, just the value.
* wf:qs(ID) returns a list.
* wf:depickle(Data, TTL) returns undefined if expired.

### Comet Pools

* Behind the scenes, combined logic for comet and continue events. This now all goes through the same channel. You can switch async mode between comet and intervalled polling by calling wf:switch_to_comet() or wf:switch_to_polling(IntervalMS), respectively.
* Comet processes can now register in a local pool (for a specific session) or a global pool (across the entire Nitrogen cluster). All other processes in the pool are alerted when a process joins or leaves. The first process in a pool gets a special init message.
* Use wf:send(Pool, Message) or wf:send_global(Pool, Message) to broadcast a message to the entire pool.
* wf:comet_flush() is now wf:flush()

### Architectural Changes

* Nitrogen now runs _under_ other web frameworks (inets, mochiweb, yaws, misultin) using simple_bridge. In other words, you hook into the other frameworks like normal (mochiweb loop, yaws appmod, etc.) and then call nitrogen:run() from within that process.
* Handlers are the new mechanism to extend the inner parts of Nitrogen, such as session storage, authentication, etc.
* New route handler code means that pages can exist in any namespace, don't have to start with /web/... (see dynamic_route_handler and named_route_handler)
* Changed interface to elements and actions, any custom elements and actions will need tweaks.
* sync:go() recompiles any changed files more intelligently by scanning for Emakefiles.
* New ability to package Nitrogen projects as self-contained directories using rebar.

# Nitrogen 1.x Changelog

## 2009-05-02

* Added changes and bugfixes by Tom McNulty.

## 2009-04-05

* Added a templateroot setting in .app file, courtesy of Ville Koivula.

## 2009-03-28

* Added file upload support.

## 2009-03-22 

* Added alt text support to #image elements by Robert Schonberger.
* Fixed bug, 'nitrogen create (PROJECT)' now does a recursive copy of the Nitrogen support files, by Jay Doane.
* Added radio button support courtesy of Benjamin Nortier and Tom McNulty.

## 2009-03-16

* Added .app configuration setting to bind Nitrogen to a specific IP address, by Tom McNulty.

## 2009-03-08

* Added DatePicker element by Torbjorn Tornkvist.
* Upgrade to JQuery 1.3.2 and JQuery UI 1.7.
* Created initial VERSIONS file.
* Added code from Tom McNulty to expose Mochiweb loop.
* Added coverage code from Michael Mullis, including lib/coverize submodule.
* Added wf_platform:get_peername/0 code from Marius A. Eriksen.

## 2009-03-07

* Added code by Torbjorn Tornkvist: Basic Authentication, Hostname settings, access to HTTP Headers, and a Max Length validator.

## 2009-01-26

* Added Gravatar support by Dan Bravender.

## 2009-01-24

* Add code-level documentation around comet.
* Fix bug where comet functions would continue running after a user left the page.
* Apply patch by Tom McNulty to allow request object access within the route/1 function.
* Apply patch by Tom McNulty to correctly bind binaries.
* Apply patch by Tom McNulty for wf_tags library to correctly handle binaries.

## 2009-01-16

* Clean up code around timeout events. (Events that will start running after X seconds on the browser.)

## 2009-01-08

* Apply changes by Jon Gretar to support 'nitrogen create PROJECT' and 'nitrogen page /web/page' scripts.
* Finish putting all properties into .app file. Put request/1 into application module file.
* Add ability to route through route/1 in application module file.
* Remove need for wf_global.erl
* Start Yaws process underneath the main Nitrogen supervisor. (Used to be separate.)

## 2009-01-06

* Make Nitrogen a supervised OTP application, startable and stoppable via nitrogen:start() and nitrogen:stop().
* Apply changes by Dave Peticolas to fix session bugs and turn sessions into supervised processes.

## 2009-01-04

* Update sync module, add mirror module. These tools allow you to deploy and start applications on a bare remote node.

## 2009-01-03 

* Allow Nitrogen to be run as an OTP application. See Quickstart project for example.
* Apply Tom McNulty's patch to create and implement wf_tags library. Emit html tags more cleanly.
* Change templates to allow multiple callbacks, and use first one that is defined. Basic idea and starter code by Tom McNulty.
* Apply Martin Scholl's patch to optimize copy_to_baserecord in wf_utils.

