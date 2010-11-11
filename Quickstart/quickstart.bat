@echo off

set ERL="erl.exe"

echo Build Nitrogen...
pushd ..\apps\nitrogen
%ERL% -make
popd

echo Build Simple Bridge...
pushd ..\apps\simple_bridge
%ERL% -make
popd

echo Build NProcReg... 
pushd ..\apps\nprocreg
%ERL% -make
popd

echo Build the Quickstart...
%ERL% -make

echo Copy static files...
rd /q /s static\nitrogen
xcopy /q /y /e /s "..\apps\nitrogen\www\*" "static\nitrogen\"

set PA1=".\ebin"
set PA2="..\apps\nitrogen\ebin"
set PA3="..\apps\nitrogen\include"
set PA4="..\apps\simple_bridge\ebin"
set PA5="..\apps\simple_bridge\include"
set PA6="..\apps\nprocreg\ebin"
set PA7="..\apps\nprocreg\include"
@echo on
%ERL% -name "nitrogen@127.0.0.1" -pa %PA1% -pa %PA2% -pa %PA3% -pa %PA4% -pa %PA5% -pa %PA6% -pa %PA7% -eval "application:start(nprocreg)" -eval "application:start(quickstart)"