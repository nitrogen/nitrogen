#!/bin/sh

# This will take the file start.cmd.src and create a new file: start.cmd

# Parse out release and erts info
START_ERL=`cat releases/start_erl.data`
ERTS_VSN=${START_ERL% *}
APP_VSN=${START_ERL#* }

cat start.cmd.src | sed "s/{ERTS_VSN}/$ERTS_VSN/g" | sed "s/{APP_VSN}/$APP_VSN/g" > start.cmd
