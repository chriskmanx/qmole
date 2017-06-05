#! /bin/sh

./test-properties-server &

until test -s iorfile; do sleep 0; done

if ./test-properties-client; then
	rm iorfile
else
	rm iorfile
	exit 1
fi
