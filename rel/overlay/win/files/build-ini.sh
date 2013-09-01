#!/bin/sh
## erl.ini rebuilder
##
## This script exists because on Windows, certain dependencies fail with
## "cannot find erlexec.exe", and because reltool doesn't copy the erl.ini file
## from the standard ERTS (nor should it, since it's pointing at the wrong
## locations for this particular release), we generate it based on the current
## directory.
##
## Further, if you were to move the nitrogen directory to a different location,
## the previously generated INI files would again be pointing at the wrong
## location, so we rebuild it every time before we compile.

WINPWD=`cmd /c "echo %cd%" | sed -e "s/\\\\\\\\/\\\\\\\\\\\\\\\\/g"`
ERTS=`ls | grep erts-`
INIFILE="$ERTS/bin/erl.ini"

echo "Generating new erl.ini file in $INIFILE"

echo "[erlang]" > $INIFILE
echo "Bindir=$WINPWD\\\\$ERTS\\\\bin" >> $INIFILE
echo "Progname=erl" >> $INIFILE
echo "Rootdir=$WINPWD" >> $INIFILE
