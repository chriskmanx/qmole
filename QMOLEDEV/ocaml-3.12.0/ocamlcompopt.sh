#!/bin/sh

topdir=`dirname $0`

exec $topdir/boot/ocamlrun $topdir/ocamlopt -nostdlib -I $topdir/stdlib "$@"
