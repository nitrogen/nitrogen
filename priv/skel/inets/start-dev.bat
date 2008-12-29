@echo off

echo Starting Nitrogen on Inets...
erl -make
erl -name nitrogen@localhost -pa ebin include -pa ..\ebin ..\include -sync_environment development -s inets_helper