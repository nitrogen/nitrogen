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
   R15B) for Windows into `c:\erlX.Y`.
2. Make sure `c:\erlX.Y\bin` is in your windows PATH (Right click My Computer,
   Click Properties, 
3. Get [msysGit](https://github.com/msysgit/msysgit/releases/download/Git-1.9.4-preview20140815/msysGit-netinstall-1.9.4-preview20140815.exe) and
   install to `c:\msysgit` (the default).

## To Build Nitrogen

1. Fire up msysgit (c:\msysgit\msys.bat)
2. Navigate to the directory where you'd like to install Nitrogen (e.g.
   `c:\www`)
3. Run `git clone https://github.com/nitrogen/nitrogen`
4. Run `cd nitrogen`
5. Run `make rel_inets_win` or (`make rel_mochiweb_win`)
6. When it completes, you'll have a standalone installation in rel/nitrogen
7. You can close Msysgit now

**Note:** You only need to use msysgit for when you want to *build* Nitrogen.
Once it's built, you can run Nitrogen from the normal Windows Explorer.

## Running Nitrogen

1. Close Msysgit and open Windows Explorer to where you installed Nitrogen
2. Run (double-click) `start.cmd`
3. Navigate your browser to `http://127.0.0.1:8000`


I realize this is not exactly the *simplest* set of instructions, but it allows
us to use some build tools otherwise inconvenient to use on windows.  As stated
above, Windows support is **EXPERIMENTAL** so, please understand that.
