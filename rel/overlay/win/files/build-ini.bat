@echo off
REM erl.ini rebuilder
REM 
REM This script exists because on Windows, certain dependencies fail with
REM "cannot find erlexec.exe", and because reltool doesn't copy the erl.ini file
REM from the standard ERTS (nor should it, since it's pointing at the wrong
REM locations for this particular release), we generate it based on the current
REM directory.
REM 
REM Further, if you were to move the nitrogen directory to a different location,
REM the previously generated INI files would again be pointing at the wrong
REM location, so we rebuild it every time before we compile.

for /f "tokens=1" %%i in ('dir /x /b erts-*') do SET ERTS=%%i

set INIFILE=%ERTS%\bin\erl.ini
set PWD=%cd:\=\\%

echo Generating new erl.ini file in %INIFILE%

echo [erlang]>%INIFILE%
echo Bindir=%PWD%\\%ERTS%\\bin>>%INIFILE%
echo Progname=erl>>%INIFILE%
echo Rootdir=%PWD%>>%INIFILE%
