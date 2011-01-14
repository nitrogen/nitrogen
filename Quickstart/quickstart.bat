@echo off

set ERL="erl.exe"

echo Build Nitrogen...
pushd ..\deps\nitrogen
%ERL% -make
popd

echo Build Simple Bridge...
pushd ..\deps\simple_bridge
%ERL% -make
popd

echo Build NProcReg... 
pushd ..\deps\nprocreg
%ERL% -make
popd

echo Build the Quickstart...
%ERL% -make

echo Copy static files...
rd /q /s static\nitrogen
xcopy /q /y /e /s "..\deps\nitrogen\www\*" "static\nitrogen\"

set PA1=".\ebin"
set PA2="..\deps\nitrogen\ebin"
set PA3="..\deps\nitrogen\include"
set PA4="..\deps\simple_bridge\ebin"
set PA5="..\deps\simple_bridge\include"
set PA6="..\deps\nprocreg\ebin"
set PA7="..\deps\nprocreg\include"
@echo on
%ERL% -name "nitrogen@127.0.0.1" -pa %PA1% -pa %PA2% -pa %PA3% -pa %PA4% -pa %PA5% -pa %PA6% -pa %PA7% -eval "application:start(nprocreg)" -eval "application:start(quickstart)"