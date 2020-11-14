# Compiling Nitrogen on Windows

**Windows support is *experimental* in Nitrogen.**

### Notes:

**Note 1:** This process can be a hair tedius, but will allow you to have the
latest and greatest version of Nitrogen from Github.com.  For the version that
doesn't require you to screw around with installing msysgit, download the
prepackaged Windows zip file from the [Nitrogen Home
Page](http://www.nitrogenproject.com).

**Note 2:** To simplify the releasing of Nitrogen on Windows, some significant
changes have been made to the build process. The previous method required a lot
of tinkering with things to build with a new release of Erlang.

**Note 3:** Throughout this readme, we recommend installing to `c:\erlX.Y`
(where X.Y is the version, for example, `5.10`) for the sake of simplicity.
It's not critical that you use this directory, but -- and it should go without
saying -- if you choose to install into another directory, make sure you
reference that directory everywhere.  The only **strong** recommendation we
make is to avoid paths with spaces in them (e.g.  `c:\program files\erlX.Y`).
Msysgit has a bit of a history of not playing nice with paths with spaces in
them.

**Note 4:** While installing Msysgit, make sure you choose the option, if
presented, to "Check out as-is and Check in as-is". Otherwise, some of the
Nitrogen prerequisits don't play nice with Windows-style line-endings (`\r\n`).

## Prerequisite Steps:

1. Install [Erlang](http://www.erlang.org/download.html) (Recommended at least
   Version 21) for Windows into `c:\erlX.Y`.
2. Make sure `c:\erl-X.Y\bin` is in your windows `PATH` (Windows Key - Type
   "path", and click the option that says "Edit the system environment variables,"
   then click "Environment Variables", and edit the `PATH` entry under "System
   Variables" to include `c:\erl-X.Y\bin` (make sure `X.Y` is the actual installed
   version (e.g. `erl-23.1`)
3. Install the [Git for Windows SDK](https://github.com/git-for-windows/build-extra/releases/tag/git-sdk-1.0.8).
   This will take a few minutes, be patient (it clones and builds a 

## To Build Nitrogen

1. Fire up `msys2` from where Git for Windows installed (likely `c:\git-sdk-64\msys2.exe`)
2. Navigate to the directory where you'd like to install Nitrogen (e.g.
   `c:\www` - you can use `cd /c/www` and it will figure out `/c` means `c:`)
3. Run `git clone https://github.com/nitrogen/nitrogen`
4. Run `cd nitrogen`
5. Run `make rel_inets_win`, `make rel_mochiweb_win`, `make_rel_cowboy_win`,
   or `make_rel_webmachine_win. When it completes, you'll have a standalone installation in `../myapp`
7. You can close `Msys2`

**Note:** You only need to use `msys2` for when you want to *build* Nitrogen.
Once it's built, you can run Nitrogen from the normal Windows Explorer.

## Running Nitrogen

1. Close `Msys2` and open Windows Explorer to where you installed Nitrogen
2. Run (double-click) `start.cmd`
3. Navigate your browser to `http://127.0.0.1:8000`


I realize this is not exactly the *simplest* set of instructions, but it allows
us to use some build tools otherwise inconvenient to use on windows.  As stated
above, Windows support is **EXPERIMENTAL** so, please understand that.
