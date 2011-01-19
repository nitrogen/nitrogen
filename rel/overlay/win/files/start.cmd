@echo off
set PA=
set PA=%PA% ./site/ebin
set PA=%PA% ./site/include
set PA=%PA% ./lib/nitrogen-2.0.3/ebin
set PA=%PA% ./lib/simple_bridge-1.0/ebin
set PA=%PA% ./lib/nprocreg-0.1/ebin
erts-5.7.5\bin\erl -pa %PA% -make
erts-5.7.5\bin\werl -pa %PA% -boot releases/2.0.3/start -embedded -config etc/app.config -args_file etc/vm.args