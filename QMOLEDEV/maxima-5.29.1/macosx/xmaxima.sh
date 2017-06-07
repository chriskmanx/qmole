#!/bin/bash

SCRIPT="${BASH_SOURCE[0]}"
while [ -L "$SCRIPT" ] ; do SCRIPT=`(readlink "$SCRIPT")` ; done

ROOT=`(cd \`dirname "$SCRIPT"\` > /dev/null 2>&1 ; pwd)`

VERSION=`ls "$ROOT/maxima/share/maxima/"`
MAXIMA_XMAXIMADIR=$ROOT/maxima/share/maxima/$VERSION/xmaxima
export MAXIMA_XMAXIMADIR
XMAXIMA_MAXIMA=$ROOT/maxima.sh

PATH="$MAXIMA_PREFIX/bin:$PATH"
export PATH

exec wish $ROOT/maxima/bin/xmaxima "$@"
