@echo off

echo Run this script from %NITROGENHOME%\rel\overlay_win, otherwise results are unpredictable.
pause

set ERL="erl.exe"
set ERTS="C:\Program Files\erl5.7.5\erts-5.7.5"

FOR /D %%f in (..\..\apps\*) DO (
    echo Building %%f...
    pushd %%f
    %ERL% -make
    popd
)

erl -args_file paths.arg -noshell -eval "systools:make_script(\"nitrogen\", [local])" -eval "systools:make_tar(\"nitrogen\")" -eval "init:stop()"

echo Generate the Nitrogen Package...
rmdir /q /s nitrogen
mkdir nitrogen
tar -zxvf nitrogen.tar.gz -C nitrogen
xcopy /y /e /s %ERTS%\*.* nitrogen\erts-5.7.5\
xcopy /y /e /s ..\overlay\etc\*.* nitrogen\etc\
xcopy /y /e /s ..\overlay_inets\etc\*.* nitrogen\etc\
xcopy /y /e /s ..\overlay\site\*.* nitrogen\site\
xcopy /y /e /s ..\overlay_inets\site\*.* nitrogen\site\
xcopy /y /e /s ..\..\apps\nitrogen\include\*.* nitrogen\lib\nitrogen-2.0.3\include\
xcopy /y /e /s ..\..\apps\simple_bridge\include\*.* nitrogen\lib\simple_bridge-1.0\include\
xcopy /y /e /s files\*.* nitrogen\
xcopy /y /e /s ..\..\apps\nitrogen\www\*.* nitrogen\site\static\nitrogen\
del nitrogen\site\Emakefile
del nitrogen.boot
del nitrogen.script
del nitrogen.tar.gz