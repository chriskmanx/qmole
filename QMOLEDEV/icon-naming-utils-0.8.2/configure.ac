dnl Process this file with autoconf to produce a configure script.
AC_PREREQ(2.52)

AC_INIT([icon-naming-utils], [0.8.2])
AC_CONFIG_SRCDIR(icon-name-mapping.pl.in)
AC_COPYRIGHT([Copyright 2005-2007 Novell, Inc.])

AM_INIT_AUTOMAKE

AM_MAINTAINER_MODE

AC_PATH_PROG(PERL, perl)
if test -z "$PERL"; then
   AC_MSG_ERROR([perl not found])
fi
if test -z "`$PERL -v | fgrep 'v5.' 2> /dev/null`" -a -z "`$PERL -v | fgrep 'version 5.' 2> /dev/null`"; then
   AC_MSG_ERROR([perl 5.x required for icon-naming-utils])
fi
AC_MSG_CHECKING([for XML::Simple])
if `$PERL -e "require XML::Simple" 2>/dev/null`; then
   AC_MSG_RESULT([ok])
else
   AC_MSG_ERROR([XML::Simple perl module is required for icon-naming-utils])
fi

AC_CONFIG_FILES([
Makefile
icon-naming-utils.pc
])

AC_OUTPUT

