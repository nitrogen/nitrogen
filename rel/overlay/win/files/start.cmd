@echo off
set PA=
set PA=%PA% ./site/ebin
set PA=%PA% ./site/include
erts-5.9\bin\erl -pa %PA% -make
erts-5.9\bin\werl -pa %PA% -boot releases/2.1.0/start -embedded -config etc/app.config -args_file etc/vm.args
