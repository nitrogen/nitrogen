@echo off

echo Copy Nitrogen WWW files into .\content\wwwroot\nitrogen
rmdir /s /q .\content\wwwroot\nitrogen
mkdir .\content\wwwroot\nitrogen
copy ..\www\* .\content\wwwroot\nitrogen

echo Starting Nitrogen on Inets...
erl -make
erl -name nitrogen@localhost -pa ebin include -pa ..\ebin ..\include -sync_environment development -s inets_helper