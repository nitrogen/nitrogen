@echo off

echo Copy Nitrogen WWW files into .\content\wwwroot\nitrogen
rmdir /s /q .\wwwroot\nitrogen
mkdir .\wwwroot\nitrogen
copy ..\www\* .\wwwroot\nitrogen

echo Starting Nitrogen on Inets...
erl -make
erl -name nitrogen@localhost -pa apps ebin include -pa ..\ebin ..\include -eval "application:start(quickstart_inets)"
