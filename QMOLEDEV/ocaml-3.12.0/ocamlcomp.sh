#!/bin/sh

topdir=`dirname $0`

exec $topdir/boot/ocamlrun $topdir/ocamlc -nostdlib -I $topdir/stdlib "$@"
