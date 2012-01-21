Windows support is experimental in Nitrogen.

To get going, you probably need the following installed:

- Erlang R15B (http://erlang.org/download.html)
- Git (http://code.google.com/p/msysgit/downloads/list?can=3)
- Open SSL (http://gnuwin32.sourceforge.net/packages/openssl.htm)

This assumes that Erlang is installed in C:\erl5.9 and is in the path.

Steps:

1. Run build.cmd from the rel\overlay\win directory. This creates a self
   contained .\nitrogen directory.

2. In the nitrogen directory, run start.cmd. This will start Nitrogen.

3. Open your browser to http://localhost:8000
