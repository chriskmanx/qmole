#!/bin/sh

autoreconf --force --install || exit 1
./configure "$@" || exit 1
