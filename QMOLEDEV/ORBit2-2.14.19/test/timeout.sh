#!/bin/sh

# This will test GIOP timeouts

echo "Running timeout server..."
./timeout-server &
server=$!

sleep 1
 
echo "Running timeout client..."
./timeout-client 
retv=$?


case "`uname`" in
Linux)
    killall lt-timeout-server
    ;;
MINGW*) 
    echo Please terminate timeout-server.exe in Task Manager.
    wait $server
    ;;
*)
    echo Please kill timeout-server and/or lt-timeout-server from another window,
    echo then type something here.
    wait $server
    ;;
esac

exit $retv
