release-ready
=============

About
-----

This is a pretty simple hack to create an rebar3 managed OTP
application instead of the standard Nitrogen layout.  In addition to
the obvious directory layout changes there are some differences
between this and a standard Nitrogen app:

*   Since this is a first class OTP application, it's possible (and
    preferable) to use `code:priv_dir/1` instead of relative
    filenames.  To this end, there's a `nitrogen_helpers` module that
    provides `template/1` and a revised `build_dispatch/0` that
    generate template paths and Cowboy static file handler paths,
    respectively.

    Therefore simple_bridge is configured to use the replacement
    `build_dispatch/0` function and the default bare.html and
    mobile.html templates demonstrate the use of
    `nitrogen_helpers:template/1`.

*   relx really wants a single sys.config file, so the three
    configuration files provided with Nitrogen (simple_bridge.config,
    app.config, and sync.config) are concatenated during compilation.

*   A custom configuration handler is installed (since the name of the
    new application probably isn't "nitrogen" and probably won't work
    with the default configuration handler.

*   The nitrogen_app.erl and nitrogen.app.src files have been replaced
    with <project>_app.erl and <project>.app.src files.  Note though
    that the standard nitrogen_sup is called.  The app.src file has
    been updated to include Nitrogen's prerequisite applications.




Usage
-----

Clone this repository then run `release-ready/new --base=<basedir> project`.  This will create a release skeleton in /path/to/basedir/project.

Next cd to the project and run `./rebar3 release` to install the dependencies, compile the code, and generate a release.

This release can be run with the command `_build/default/rel/<project>/bin/<project> console`.
