#!/bin/sh
aclocal -I config
libtoolize --force --copy
automake --foreign --add-missing --copy
autoconf

