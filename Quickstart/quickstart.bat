@echo off

echo Copy Nitrogen WWW files into .\content\wwwroot\nitrogen
rmdir /s /q .\wwwroot\nitrogen
mkdir .\wwwroot\nitrogen
copy ..\apps\nitrogen\www\* .\wwwroot\nitrogen

echo Compile dependencies...
cd ..\apps\nitrogen
erl -make
cd ..\simple_bridge
erl -make
cd ..\..\Quickstart

echo Starting Nitrogen on Inets...
erl -make
erl -name nitrogen@127.0.0.1 -pa apps ebin include ..\apps\nitrogen\ebin ..\apps\nitrogen\include ..\apps\simple_bridge\ebin ..\apps\simple_bridge\include -eval "application:start(quickstart_inets)"
