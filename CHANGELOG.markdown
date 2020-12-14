# Nitrogen 2.x

## Nitrogen 2.4.1 (in progress)

* Added Content Security Policy functionality (@bunnylushington)
* Added a new Postback handler. This is the new last handler to process
  anything, and gives the user a change to capture, modify, or do anything else
  to a postback request before it gets processed. (@bunnylushington)
* Added `#template.callouts` attribute to enable or disable callout
  functionality. (Setting `callouts` to `false` will result in the template to
  not process `[[[mod:fun()]]]` blocks).
* Added `#template.text` attribute, which allows to in-line templates.
  Basically, the `text` attribute will be used as the `body` of the template.
  (Thanks @bunnylushington)
* Added a `#delay_body.method`, which may selectively be used to determine if s
  `simple` method (that may be less optimized, but more universally acceptable)
  can be used.  This is the method used in `comet`, and is also recommended if
  the `#delay_body` element will have its rendered results cached.
* Fix `#delay_body` crash if `delegate` is not specified.
* When using caching, you're able to run `wf_context:cache()` to determine if
  the current operation is being run while being cached, or just being run
  normally.
* Nitrogen can now generate documentation for [Dash](https://kapeli.com/dash)
  (OSX) and [Zeal](https://zealdocs.org/) (Linux/Windows). Big thanks to
  @bunnylushington for this.

## Nitrogen 2.4.0

* Cache Handler handler implemented with [Nitro Cache](https://github.com/nitrogen/nitro_cache),
  a fork of [Simple Cache](https://github.com/marcelog/simple_cache), modified
  with a mutex system to prevent parallel calculation of the same keys for use
  with Nitrogen.
* Added `wf:cache/[1,2,3]`, `wf:set_cache/[2,3]` and `wf:clear_cache/[1,2]`
* Added a new `plugin` command to the `bin/dev` script to initialize a new
  plugin (@fbrau)
* Improved performance by no longer relying on mochiglobal in templates (which
  was very slow for sets, but plenty fast for gets), replaced by using the new
  cache candler, which is very fast for both gets and sets.
* Added a record extension capability through the use of the `rekt` parse
  transform.  This allows quickly creating new elements from old ones, sort of
  like inheritance.
* All documentation has been converted from org-mode to markdown for easier
  editing.
* The `#template{}` element is now enabled to use the `pandoc` utility (if it's
  on the system) to load and convert from all sorts of files (markdown, HTML,
  LaTeX, epub, PDF, etc).
* Added `wf:parse_qs/1`, which takes a URL-Encoded query (or post) string and
  returns a proplist.
* Nitrogen template callouts now be used to render variables, rather than just
  function calls. For example: `[[[MyVar]]]`
* Added a new settings file for vim users. Can be installed to `.vimrc` by
  running `make install-vim-script`
* Added a support for [Mermaid](https://mermaid-js.github.io/mermaid/#/) with
  a `#mermaid{}` element. (@joaohf)
* Added `#js_fun{}` action to aid calling javascript functions with properly
  escaped arguments.
* Added `#youtube{}` and `#video{}` elements.
* Added `wf:q_map()` and `wf:q_maps()` API calls to retrieve collections of 
  form fields as maps.
* Added `wf:in_request()` which will return `true` if the current process is
  actually a Nitrogen request.
* Added `#date_dropdown{}` element, which is an alternative date picker ideal
  for dates of birth (where you don't need a calendar visual).
* Added [qdate](https://github.com/choptastic/qdate) as a default dependency
  for use with `#date_dropdown{}`.
* Added `#remove_option{}`, `#add_option{}`, `#disable_option{}`, and
  `#enable_option{}` for interfacing with `#dropdown{}` options.
* Added `#open_window{}` to open new windows.
* Added `#if_value{}` action, which lets you wrap other actions with
  conditionals based on the value of form elements.
* Added `#is_number{}` validator, which works exactly like `#is_integer{}`
  except it allows floats.
* Removed a number of deprecated `erlang:now()` calls.
* Added `wf_test:[en|dis]able_autoadvance`, preventing a test from advancing to
  the next test on its own - this facilitates relying on the page itself
  manually initiate the redirect.
* Added `test_options` to the test system, allowing to specify certain options
  for initializing and running the test. Currently, the only `test_option` is
  `base_url` for setting prefix of the URL (ie, `https://my-domain-name.com`).
* Add `wf:render_isolated/1`, which returns a tuple containing the `{HTML, JS}`,
  where `HTML` is the rendered HTML from the Nitrogen elements, and `JS` is the
  queued javascript that was either attached directly to the elements, or wired
  in the elements' render functions.
* Added `login` attribute to `#redirect{}` element. (Alexander Sedov)
* Updated the `#radiogroup` element such that it will now do a deep dive of
  containing elements to update the `name` attributes.  This eliminates the
  need to use a flattened list and makes the `#radiogroup` element much more
  useful.
* Fix bug related to `#dropdown{}` option ambigious item selection. (Help from
  Stuart Thackray)
* Fix a handful of issues related to unicode
* Fix a process leak in comet and `#sync_panel{}`
* Upgrade included rebar to a post-2.6.0 version that includes support for
  Rebar3-type deps (This will be the last non-bugfix release that uses rebar2).
* Fix Bert encoding for string longer than 65535 characters.
* Added binary builds for Raspberry Pi again.
* `wf:url_encode/1` will now properly handle floats.
* Refactor `wf_render_elements:render_element/1` from a single monolothic
  function into a bunch of smaller parts.
* Fix `wf:json_encode/1` issue related to some structures not properly removing
  `{struct` tuples.
* Fix bug related to improperly selected elements in a `#dropdown{}` element
  (Stuart Thackray)
* Add experimental support for Cowboy 2.x's sendfile (@rkulkarni)
* Upgrade to Cowboy 2.7.0 (@etxemag75)
* Upgrade to Yaws 2.0.8 (Stuart Thackray)
* Upgrade to Mochiweb 2.20.1
* Upgrade to Webmachine 1.11.1
* Add support for Erlang 23 (@etxemag75)
* Add support for UTF-16 (@miby00)
* SimpleBridge to version 2.1.0 (most critically, fixes a number of crashes)
* nprocreg to version 0.3.0 to improve performance.
* jQuery to version 3.5.1
* jQuery Mobile's elements have been deprecated and will be removed in
  Nitrogen 3.0 and moved into a plugin (@fbrau)
* Various fixes for deprecated modules and functions (crypto, rand, inets, etc)

## Nitrogen 2.3.1

* Make return values of `wf_convert:parse_ip/1` consistent.
* Update to SimpleBridge 2.0.1 (Fixes an inets header bug)
* Fixed issue where clicking a link to download a file would stop the event
  loop on the client.
* Added a URL to the error message printed by the `debug_crash_handler`.

## Nitrogen 2.3.0

* Upgrade to SimpleBridge 2.0:
  + Eliminate platform-specific code in Nitrogen, relying completely on
    SimpleBridge for setting up underlying server.
  + Added websockets as an alternative connection mode for `wf:comet` and its
	siblings as well as for postbacks. If a websocket connection is unable to
	be established, AJAX and Comet will be used.
  + Uploads still use AJAX.
  + Remains backwards compatible with versions of Nitrogen prior to 2.3,
	however, websockets will not be available unless a proper upgrade is
    performed.
* Added a testing framework that runs on the client and the server through
  Nitrogen's postback system, ensuring that you can add automated testing which
  will verify your interface in the browser of your choice.
* To go along with the testing framework are a number of initial tests to test
  the core of the Nitrogen framework. These can be found in the src/tests
  directory of the
  [NitrogenProject.com](https://github.com/nitrogen/NitrogenProject.com.git)
  repository.
* Significantly improve the comet/async reconnection system, including a
  default notice at the top of the page, and the ability to specify manual
  reconnection actions (if you so desire).  This is done with
  `#comet.reconnection_actions` attribute.
* Update the build process to support `PROJECT` and `PREFIX` variables to
  create a new project. By default, the build process (`make rel_X` without any
  variables specified) will create the app at `../myapp`.
* Added "Smart Extensions" to the default route handler (controlled by
  application configuration) which allows specifying custom entry points into
  page modules depending on the extension of the submission, as well as a
  JSON smart extension.
* Added a simple REST behaviour to the default route handler. Adding
  `-behaviour(nitrogen_rest)` will will enable entry functions into the module
  based on the request type (example: `GET` requests will call
  `PageModule:get/0`, etc).
* Added `#sync_panel` element, which allows for simple real-time content
  synchronization over comet/websockets.
* Added `#progress_bar` element.
* Added a `debug_crash_handler`, which is enabled by default in new Nitrogen
  projects. This will print stacktraces to the reply, or to the client's
 `console.log` (if the borwser supports it). Helps with debugging.
* Updated and improved the `#upload` element, adding progress bars, delete
  buttons, and proper enforcement of the `multiple` option.
* Added `#qr` element for simple QR code generation (which uses Google Charts).
* Added `wf:set_multiple/2` call and related `#set_multiple` action for setting
  multiple values in `#dropdown.multiple` elements.
* Added `html_id` and `title` attributes to all elements, corresponding with
  the HTML `id` and `title` attributes (Amos Oviedo)
* Normalize the `next` attribute on input elements to use tab instead of enter,
  and also add `next` to elements where it was previously missing
  (Stuart Thackray)
* Added `role` attribute to remaining HTML5 elements. (Aaron Frantisak)
* Added `#iframe{}` element. (Aaron Frantisak)
* Update Bert.js and `#api{}` action to use binaries for proplist keys instead
  of atoms inconjuction with the `safe` option for `term_to_binary` preventing
  possible crash related to atom table limit.
* Added `distance` and `options` attributes to `#draggable` to minimize
  accidental clicking and dragging when the cursor only moves a few pixels.
* Improved Unicode handling - messages between client and server will always
  convert UTF8. Update Bert.js to support unicode strings.
* Improve template system to use binaries instead of strings, improving
  template rendering performance.
* Allow validation to be applied to `#radiogroup` element.
* Added `unless_has_value` option to the `#is_required` validator, which links
  multiple fields together, allowing options such as "Please enter either
  FieldX or FieldY".
* Added `when_empty` attribute to the `#js_custom` validator, giving the option
  to validate a custom javascript validator even if the value is empty.
* Added `trap_tabs` attribute to `#textarea` to allow user to specify if the tab
  key should move focus to the next element (default behavior) or insert a tab
  into the textarea.
* Added `wf:to_qs/1` function for converting a proplist of elements into a
  URL-encoded querystring.
* Added `wf:to_existing_atom/1`.
* Added `wf:json_encode/1` and `wf:json_decode/1`.
* Added `wf:download_as/1` convenience function for specifying the
  `Content-Disposition` header so the client will download a page with
  specified filename.
* Added `wf:protocol/0` function to retrieve the protocol used (http or https)
* Added `wf:url/0` function to get the full request path, including protocol
  and host name.
* Added a `#click{}` action, which when called will initiate a click on the
  target element.
* Added an `enter_clicks` attribute to `#button{}` which will allow simpler
  form submission, as pressing "enter" when the element is selected will click
  the linked button, initiating a postback or other action as necessary.
* Added a helper script for fixing a Nitrogen slim release if the version of
  Erlang installed on the machine and the version of Erlang the slim release
  depends on are different.
* Updated `wf:html_encode` to be more flexible (preventing crashes when a user
  accidentally puts tuples, records, or anything else in an element's `text`
  field). Also added some tests for `html_encode`.
* Added `wf:encode/{0,1}` to specify any custom response encoding to be applied
  before sending to the client.
* Added `body` to `#radio` and `#restful_submit` (Stuart Thackray)
* Added `button_class` to `#recaptcha{}` (Stuart Thackray)
* Added minified versions of included javascript versions as well as
  non-minified
  versions.
* Added `Nitrogen.$eval()` to nitrogen.js to allow you to control when to
  evaluate postback responses.
* Significantly reworked the `#textbox_autocomplete{}` element to simplify its
  implementation (no more `{struct` tuples for mochijson compliance) and rework
  the associated demo to NitrogenProject.com.
* Fixed rendering bug in `#upload` when it would start hidden by CSS.
* Fixed issue with POST size in Yaws (Steve Vinoski)
* Updated to work with Erlang/OTP 17+
* Fixed the use of `wf:qs/1` with `#dropdown{multiple=true}`
* Improve a handful of error messages (comet pool unexpected death and action
  module failing to load).
* Fixed default configuration for Yaws to allow larger POST values
* Fixed compatibility issues with Windows Bash and the `dev` script.
  (Alex Popov)
* Fixed issue with the combination of a REST handler combined with the HTTP
  auth security handler (Xue Hongwei)
* Update to jQuery and jQuery UI 1.11.1
* Update to jQuery Mobile 1.4.4 (Alex Popov)
* Add jQuery Touch Punch for better touchscreen support for drag, drop, and
  sort.
* Webserver Upgrades:
  + Cowboy => 1.0.1 (Amos Oviedo)
  + Mochiweb => 2.9.1
  + Yaws => 1.9.8
  + Webmachine => 1.10.6
* More documentation improvements and new
  [demos](http://nitrogenproject.com/demos) on the homepage.

## Nitrogen 2.2.2

* Fix an infinite loop bug in `#mobile_grid`.
* Plugins now support more OTP-compliant static directory structure with
  `priv/static` instead of just `static` on the root of the plugin directory.
* Plugins now support importing templates from `priv/templates`, added
  `template_dir` option to plugins.config
* Modify `#checkbox` to render the HTML checkbox inside its associated label
  for better semantics.
* Bugfix for AES pickling when signkey specified is incorrect size for AES.
* Update to SimpleBridge 1.4.0

## Nitrogen 2.2.1

* Add Secure Pickling with AES (the core of the operation inspired by N2O's
  version, though changes had to made to ensure backwards compatibility with
  R15B, including an include generator for dealing with crypto changes from
  R15B to R16B01)
* Add fields that were intended to be added in 2.2.0 to `#datepicker_textbox`
  to make it consistent with `#textbox`.
* Fix bug in `#upload` that was emitting the wrong formatting for `multiple`
  attribute
* Fix bug in `dev` script related to slim releases
* Fix in `#make_writable` and `#make_readonly`
* Added and updated docs

## Nitrogen 2.2.0

* Added plugin system for specifying Nitrogen elements as rebar dependencies,
  and then copying static resources, and linking in necessary headers.
  * Available plugins:
    https://github.com/nitrogen/nitrogen/wiki/Nitrogen-Plugins
  * Sample Plugin: https://github.com/nitrogen/sample_nitrogen_plugin
* Added `Module:transform_element/1` as an alternative to
  `Module:render_element`. `transform_element` assumes the element is defined
  in terms of other Nitrogen elements, and will not incur the overhead
  associated with rendering an element twice.
* Added support for Erlang Slim Releases, which don't include the full ERTS
  system. Created with `make slim_X` where `X` is yaws, cowboy, etc. (e.g.
  `make slim_mochiweb`)
* Added official support for embedding Nitrogen into existing Erlang
  applications by using the `embed` script found in the root of the nitrogen
  repository.
* Added Binary distributions for FreeBSD (9.1-RELEASE) and Raspberry Pi with
  Raspbian. Added Webmachine builds to Windows.
* Added new crash handler for specifying custom page for dealing with page
  crashes (rather than simply printing "Internal Server Error").
* Removed support for any version of Erlang below R15 (due to the addition of
  the use of `-callback` instead of `behaviour_info` in handler definitions.
* Added typespecs to all built-in elements, actions, and validators. Added
  function specs in many places. Added `-callback` specs to handler behaviours.
* Added generalized `#confirm_same` validator for validating that two fields
  have the same value (useful for confirming entered email addresses match,
  etc). `#confirm_password` validation redone to use this `#confirn_same`
  validator.
* Added `dependency_js` attribute to the base action, allowing any actions that
  require certain javascript files to load the specified dependency first, then
  execute the action. This can help with load times by only loading javascript
  files on demand.
* Added `type` attribute to textbox to allow HTML5 textbox types. `#password`
  reworked to use this.
* Added height and width attributes to the `#image` element. (Maxim Sokhatsky)
* Added `wf:defer` and `wf:eager` variants to `wf:wire` to help ensure wiring
  order, and modify many helper functions (e.g. `wf:insert_after`) to specify
  priority (`eager`, `normal`, or `defer`).
* Added enhanced validation handling to allow advanced functionality to be
  executed (both server or client side) when a postback is cancelled due to a
  validation failure. (Jonas Adahl)
* Added `module_alias` option to `#template`, which allows aliasing a
  referenced module within a template, allowing templates to be "remapped" to
  more than one different modules. (e.g. In making a template call like
  `layout:some_layout_fun()` have `layout` be remapped to any module you want.
  (Joshua Pyle)
* Added `image` and `body` attributes to `#button` to allow simple
  iconification of a button, or to specify a general body in terms of HTML or
  Nitrogen Elements for button.
* Added `#enable`, `#disable` actions for dynamically enabling and disabling
  form fields. Added `wf:enable` and `wf:disable` convenience functions to go
  along with those.
* Added `#make_readonly` and `#make_writable` actions for dynamically toggling
  the readonly attribute on fields.
* Added `data_fields` attribute to the base element, and added support in all
  elements where it makes sense to support.
* Added `#sparkline` element for making quick inline graphs.
* Added `text` and `html_encode` attributes to the HTML5 `#mark` element.
* Added `make dialyzer` to `nitrogen_core`, `nprocreg`, and
  `NitrogenProject.com` as well as generated releases to help with debugging
  apps using the new record and function typespecs.
* Added `wf:redirect_to_login(Url, PostLoginUrl)` for specifying a destination
  URL after login that is not necessarily the current page.
* Reworked `#recaptcha` element: application variables are no longer required
  (instead those values can be specified in the element itself). General
  `event/1` callback replaced with recaptcha-specific `recaptcha_event/2`
  callback more consistent with other elements that work similarly.
* Reworked `wf:peer_ip/1-3` to use IPv4 and IPv6, as well as making them always
  use tuples for IP Addresses, even if provided IP Addresses are
  strings/binaries.
* Fixed Comet such that navigating back to a page that previously had comet
  running, and if the comet process itself is still alive (has not yet timed
  out on the server), the page will properly restore the comet connection
  (Dmitriy Kargapolov)
* Fixed `inplace_textarea` and `inplace_textbox` to ensure they will always
  revert to the last set value when the cancel button is pressed.
* Fixed `wf:js_escape/1` will now also escape single quotes "'".
* Fixed `#grid_clear{}` so it will no longer crash if it's the last element in
  a `#container_X` element.
* Fixed a bug in the `#range` element causing a crash when `data` attributes
  were set.
* Fixed `html_encode=whites` to be allowing of line-breaks. That is, not all
  spaces should be converted to `&nbsp;`.
* Removed the annoying debug messages from the `action_continue`.
* Removed the `#hgroup` HTML5 element, as that element was removed from the
  HTML5 spec. Not a big deal, since it was never documented in Nitrogen anyway.
* Update Mochiweb to 2.7.0
* Update Yaws to a version greater than 1.96 (with a fix for `crypto:sha`)
* Update Cowboy to 0.8.6 (Thanks Roman Shestakov for doing SimpleBridge's API
  changes from 0.6 to 0.8)
* Update Webmachine to 1.10.4p1
* Update jQuery Mobile to 1.3.1
* Added a [Contribution
  guide](http://https://github.com/nitrogen/nitrogen/blob/master/CONTRIB.markdown)

## Nitrogen 2.1.0

* Split dependent projects into separate .git repos, including nitrogen_core,
  nprocreg, and simple_bridge
* Move Nitrogen project (plus sub-apps) to github.com/nitrogen
* Remove ./apps directory.
* Update sample project (built using `make rel_*`) to start up using a
  first-class Erlang application.
* Sync has been split into its own dependency
* Add jQuery Mobile support and elements (with help from Mattias Holmlund)
* Add Mobile sample page to default installation
* Improved Windows compilation, including official support for Mochiweb on
  Windows
* Added ability to clear validators
* Added RESTful form elements (Steffan Panning)
* Added an `html_id` attribute to most elements that would need to specify an
  HTML id attribute (Jeno Hajdu)
* Fixed a critical comet bug causing comet processes to not die 
* Add file drag/drop and multi-file capabilities to #upload element
* Add RECAPTCHA element (Steffan Panning)
* Add support for HTML5 data attributes for select elements
* Resurrect the `#wizard{}` element
* Add `module_prefix` configuration option (James Pharaoh)
* Fix bug where continuations didn't keep the original context
* Add slide up and down actions
* Make `html_encode` more forgiving
* Add helper functions for `wf:q` - `wf:mq`, `wf:q_pl`
* Add helper attribute `click` to `#button{}`
* Add `wf:join`
* Add `make upgrade` to generated releases to simplify the upgrade process from
  one version of Nitrogen to another
* Ensure `label`, `p`, `span`, and `panel` all support both `body`, `text`,
  `html_encode`
* Add more functions: `peer_ip`, `insert_before`, `insert_after`, 
* Added lots of documentation, including handlers, configuration, mobile
  elements, and restful elements.
* For improved compabitility, removed the .ez files from releases
* Added Disqus coment system to all documentation pages
* Add improved options for Webmachine routing dispatch
* Update Mochiweb dependency to 2.3.1
* Update Yaws dependency to 1.94
* Update to jQuery 1.8.2 and jQuery UI 1.8.23
* Add Cowboy 0.6.0 Webserver Support (with help from Tuncer Ayaz)
* Add Minified versions of standard Nitrogen-provided Javascript files


## Nitrogen 2.0.4

* Implement wf:flash(FlashID, Elements) so that flash notice can be
  manipulated/closed/etc.
* Add support for running Nitrogen underneath Webmachine. This makes it easier
  to run Nitrogen and Webmachine side-by-side, and allows the use of Webmachine
dispatch table to route requests to Nitrogen.
* Add ability to generate Nitrogen-Webmachine packages.
* Update Mochiweb dependency to 1.5.0.
* Update Yaws dependency to 1.89.

## Nitrogen 2.0.3

* Implement callbacks to chain Ajax effects (Jonas Ådahl)
* Fix element name normalization (Jonas Ådahl)
* Support for HTML5 semantic elements (Rajiv M Ranganath)
* Fix recv_from_socket bug when running on Yaws (Gregory Haskins)
* Update livevalidation.js dependency, fixes validation bug (Boris 'billiob'
  Faure)
* Fix bug causing postbacks to continually grow in certain cases (Jonas Ådahl)
* Added rudimentary support for building and running on Windows (Rusty
  Klophaus)
* Allow creating page, element, and action stubs from Erlang console more
  easily (Rusty Klophaus)

## Nitrogen 2.0.2

* Fixed mochiweb bug that caused other headers to get clobbered when using
  default content type.
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

* Removed mnesia-based process registry, create new distributed process
  registry. (./apps/nprocreg)
* Fixed Yaws and Mochiweb SimpleBridge to set a default Content-Type. [#60]
  [#69]
* Fixed Inets SimpleBridge to properly set headers specified as lists rather
  than atoms. [#60] [#70]
* Disabled Ajax Caching. [#45]
* Ensure site/actions and site/elements directories exist. [#49] [#75]
* Fix page prototype used by "bin/dev page <page>" [#50]
* Update #container_12{} and #container_16{} to use supplied class names and
  ids [#51]
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

* wf:wire can now act upon CSS classes or full JQuery Paths, not just Nitrogen
  elements. For example, wf:wire(".people > .address", Actions) will wire
  actions to any HTML elements with an "address" class underneath a "people"
  class. Anything on http://api.jquery.com/category/selectors/ is supported
* Added wf:replace(ID, Elements), remove an element from the page, put new
  elements in their place.
* Added wf:remove(ID), remove an element from the page.
* New #api{} action allows you to define a javascript method that takes
  parameters and will trigger a postback. Javascript parameters are
  automatically translated to Erlang, allowing for pattern matching. 
* New #grid{} element provides a Nitrogen interface to the 960 Grid System
  (http://960.gs) for page layouts.
* The #upload{} event callbacks have changed. Event fires both on start of
  upload and when upload finishes. 
* Upload callbacks take a Node parameter so that file uploads work even when a
  postback hits a different node.
* Many methods that used to be in 'nitrogen.erl' are now in 'wf.erl'. Also,
  some method signatures in wf.erl have changed.
* wf:get_page_module changed to wf:page_module
* wf:q(ID) no longer returns a list, just the value.
* wf:qs(ID) returns a list.
* wf:depickle(Data, TTL) returns undefined if expired.

### Comet Pools

* Behind the scenes, combined logic for comet and continue events. This now all
  goes through the same channel. You can switch async mode between comet and
  intervalled polling by calling wf:switch_to_comet() or
  wf:switch_to_polling(IntervalMS), respectively.
* Comet processes can now register in a local pool (for a specific session) or
  a global pool (across the entire Nitrogen cluster). All other processes in
  the pool are alerted when a process joins or leaves. The first process in a
  pool gets a special init message.
* Use wf:send(Pool, Message) or wf:send_global(Pool, Message) to broadcast a
  message to the entire pool.
* wf:comet_flush() is now wf:flush()

### Architectural Changes

* Nitrogen now runs _under_ other web frameworks (inets, mochiweb, yaws,
  misultin) using simple_bridge. In other words, you hook into the other
  frameworks like normal (mochiweb loop, yaws appmod, etc.) and then call
  nitrogen:run() from within that process.
* Handlers are the new mechanism to extend the inner parts of Nitrogen, such as
  session storage, authentication, etc.
* New route handler code means that pages can exist in any namespace, don't
  have to start with /web/... (see dynamic_route_handler and
  named_route_handler)
* Changed interface to elements and actions, any custom elements and actions
  will need tweaks.
* sync:go() recompiles any changed files more intelligently by scanning for
  Emakefiles.
* New ability to package Nitrogen projects as self-contained directories using
  rebar.

# Nitrogen 1.x Changelog

## 2009-05-02

* Added changes and bugfixes by Tom McNulty.

## 2009-04-05

* Added a templateroot setting in .app file, courtesy of Ville Koivula.

## 2009-03-28

* Added file upload support.

## 2009-03-22 

* Added alt text support to #image elements by Robert Schonberger.
* Fixed bug, 'nitrogen create (PROJECT)' now does a recursive copy of the
  Nitrogen support files, by Jay Doane.
* Added radio button support courtesy of Benjamin Nortier and Tom McNulty.

## 2009-03-16

* Added .app configuration setting to bind Nitrogen to a specific IP address,
  by Tom McNulty.

## 2009-03-08

* Added DatePicker element by Torbjorn Tornkvist.
* Upgrade to JQuery 1.3.2 and JQuery UI 1.7.
* Created initial VERSIONS file.
* Added code from Tom McNulty to expose Mochiweb loop.
* Added coverage code from Michael Mullis, including lib/coverize submodule.
* Added wf_platform:get_peername/0 code from Marius A. Eriksen.

## 2009-03-07

* Added code by Torbjorn Tornkvist: Basic Authentication, Hostname settings,
  access to HTTP Headers, and a Max Length validator.

## 2009-01-26

* Added Gravatar support by Dan Bravender.

## 2009-01-24

* Add code-level documentation around comet.
* Fix bug where comet functions would continue running after a user left the
  page.
* Apply patch by Tom McNulty to allow request object access within the route/1
  function.
* Apply patch by Tom McNulty to correctly bind binaries.
* Apply patch by Tom McNulty for wf_tags library to correctly handle binaries.

## 2009-01-16

* Clean up code around timeout events. (Events that will start running after X
  seconds on the browser.)

## 2009-01-08

* Apply changes by Jon Gretar to support 'nitrogen create PROJECT' and
  'nitrogen page /web/page' scripts.
* Finish putting all properties into .app file. Put request/1 into application
  module file.
* Add ability to route through route/1 in application module file.
* Remove need for wf_global.erl
* Start Yaws process underneath the main Nitrogen supervisor. (Used to be
  separate.)

## 2009-01-06

* Make Nitrogen a supervised OTP application, startable and stoppable via
  nitrogen:start() and nitrogen:stop().
* Apply changes by Dave Peticolas to fix session bugs and turn sessions into
  supervised processes.

## 2009-01-04

* Update sync module, add mirror module. These tools allow you to deploy and
  start applications on a bare remote node.

## 2009-01-03 

* Allow Nitrogen to be run as an OTP application. See Quickstart project for
  example.
* Apply Tom McNulty's patch to create and implement wf_tags library. Emit html
  tags more cleanly.
* Change templates to allow multiple callbacks, and use first one that is
  defined. Basic idea and starter code by Tom McNulty.
* Apply Martin Scholl's patch to optimize copy_to_baserecord in wf_utils.

