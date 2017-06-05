#! /bin/sh

# Regeneration of autoconf / automake files

rm -f config.cache
rm -f config.log
aclocal
autoconf
automake -a
