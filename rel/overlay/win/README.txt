Windows support is experimental in Nitrogen.

Note: This process can be a hair tedius, but will allow you to have the latest
and greatest version of Nitrogen from Github.com.  For the version that doesn't
require you to screw around with installing msysgit, download the prepackaged
Windows zip file from the Nitrogen home page: http://www.nitrogenproject.com

Note: To simplify the releasing of Nitrogen on Windows, some significant changes
have been made to the build process. The previous method required far too much
tinkering with things to build with a new release of Erlang.

The overview to build on windows is as follows:

Requisite (mostly one-time) steps:

1) Install Erlang (R15B) for Windows into c:\erl5.9
2) Make sure c:\erl5.9\bin is in your PATH.
3) Install msysgit (Let's say to c:\msysgit)
4) Make sure git is in your PATH as well (c:\msysgit\bin\)
4) Install mingw (Let's say to c:\mingw)
5) In msysgit, add the mingw bin paths to your PATH
6) Make sure msys's make.exe is in the path as well. c:\mingw\msys\1.0\bin

Once you've done the above, then from there, you can build Nitrogen in the usual
method:

1) Fire up msysgit
2) Navigate to the directory where you'd like to install
3) git clone https://github.com/nitrogen/nitrogen
4) cd nitrogen
5) make rel_inets_win
6) when it completes, you'll have a standalone installation in rel/nitrogen
7) Close Msysgit and open Windows Explorer to where you installed Nitrogen
8) Run start.cmd
9) Navigate your browser to http://127.0.0.1:8000


I realize this is not exactly the *simplest* set of instructions, but it allows
us to use some build tools otherwise inconvenient to use.  As stated above,
Windows support is *EXPERIMENTAL* so, please understand that.


LONG INSTRUCTIONS

Prerequisite Steps (Installing Erlang, Msysgit, and MinGW)

** Installing Erlang
