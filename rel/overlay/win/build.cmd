REM @echo off

echo Run this script from %NITROGENHOME%\rel\overlay_win, otherwise results are unpredictable.
pause

set ERL="erl.exe"
set ERTS="c:\erl5.9\erts-5.9"

FOR /D %%f in (..\..\..\deps\*) DO (
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
xcopy /y /e /s %ERTS%\*.* nitrogen\erts-5.9\
xcopy /y /e /s ..\common\etc\*.* nitrogen\etc\
xcopy /y /e /s ..\inets\etc\*.* nitrogen\etc\
xcopy /y /e /s ..\common\site\*.* nitrogen\site\
xcopy /y /e /s ..\inets\site\*.* nitrogen\site\

xcopy /y /e /s ..\..\..\deps\sync\* nitrogen\lib\sync\
xcopy /y /e /s ..\..\..\deps\nprocreg\* nitrogen\lib\nprocreg\
xcopy /y /e /s ..\..\..\deps\nitrogen_core\* nitrogen\lib\nitrogen_core\
xcopy /y /e /s ..\..\..\deps\simple_bridge\* nitrogen\lib\simple_bridge\
xcopy /y /e /s files\*.* nitrogen\
xcopy /y /e /s ..\..\..\deps\nitrogen_core\www\*.* nitrogen\site\static\nitrogen\
del nitrogen\site\Emakefile
del nitrogen.boot
del nitrogen.script
del nitrogen.tar.gz
