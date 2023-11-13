# About

This is an Erlang web application using the [Nitrogen Web
Framework](https://nitrogenproject.com).

# Building

You can rebuild the project by typing:

   make

If this fails, it likely means that its a slim release and Erlang is not
installed on this machine. Please [install
Erlang](https://www.erlang-solutions.com/resources/download.html).

# Running

You can start the application with:

  make daemon

which will start it in daemon mode.  You can attach to this started daemon
with:

  make attach

If you'd rather have an Erlang console right away, rather than starting a
daemon, type:

  make run_release

You can hot-upgrade a running release with:

  make upgrade_running

If a release fails to build or upgrade, you can revert the current version with

  make revert_version

# Config

The rebar.config file controls dependencies and build parameters using the
popular [rebar3](http://rebar3.org) tool.

The `plugins.config` file controls how Nitrogen plugins are handled.

The files in the `etc` directory control application configuration, including
ports, webserver set up, and the like.  Please see the files there for
configuration options, or view the Nitrogen documentation.

# Code

Source code can be found in `src`

Template code is in `priv/templates`

Static resources (images, js, etc) go in `priv/static`

# Live code reloading

By default Nitrogen comes with an application called
[sync](http://github.com/rustyio/sync) which provides automatic code reloading
when the related .erl file or .ebin file is updated.  To use this, run the
following on the Erlang shell:

  sync:go().
