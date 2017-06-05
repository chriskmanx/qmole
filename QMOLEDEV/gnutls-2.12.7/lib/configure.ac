dnl Process this file with autoconf to produce a configure script.
# Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008,
# 2009, 2010 Free Software Foundation, Inc.
#
# Author: Nikos Mavrogiannopoulos, Simon Josefsson
#
# This file is part of GnuTLS.
#
# The GnuTLS is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public License
# as published by the Free Software Foundation; either version 2.1 of
# the License, or (at your option) any later version.
#
# The GnuTLS is distributed in the hope that it will be
# useful, but WITHOUT ANY WARRANTY; without even the implied warranty
# of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with GnuTLS; if not, write to the Free
# Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
# MA 02110-1301, USA

AC_PREREQ(2.61)
AC_INIT([libgnutls], [2.12.7], [bug-gnutls@gnu.org])
AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_MACRO_DIR([m4])

AM_INIT_AUTOMAKE([1.10 -Wall -Werror -Wno-override])
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])
AM_CONFIG_HEADER(config.h)

AC_PROG_CC
lgl_EARLY

AC_LIBTOOL_WIN32_DLL
AC_PROG_LIBTOOL

LIBGNUTLS_HOOKS

AM_GNU_GETTEXT([external])
AM_GNU_GETTEXT_VERSION([0.17])

AC_C_BIGENDIAN

# For includes/gnutls/gnutls.h.in.
AC_SUBST(MAJOR_VERSION, `echo $PACKAGE_VERSION | sed 's/\(.*\)\..*\..*/\1/g'`)
AC_SUBST(MINOR_VERSION, `echo $PACKAGE_VERSION | sed 's/.*\.\(.*\)\..*/\1/g'`)
AC_SUBST(PATCH_VERSION, `echo $PACKAGE_VERSION | sed 's/.*\..*\.\(.*\)/\1/g'`)
AC_SUBST(NUMBER_VERSION, `printf "0x%02x%02x%02x" $MAJOR_VERSION $MINOR_VERSION $PATCH_VERSION`)
AC_CHECK_TYPE(ssize_t,
  [
    DEFINE_SSIZE_T="#include <sys/types.h>"
    AC_SUBST(DEFINE_SSIZE_T)
  ], [
    AC_DEFINE(NO_SSIZE_T, 1, [no ssize_t type was found])
    DEFINE_SSIZE_T="typedef int ssize_t;"
    AC_SUBST(DEFINE_SSIZE_T)
  ], [
    #include <sys/types.h>
  ])

# For minitasn1.
AC_CHECK_SIZEOF(unsigned long int, 4)
AC_CHECK_SIZEOF(unsigned int, 4)

AC_ARG_WITH(zlib, AS_HELP_STRING([--without-zlib],
                                 [disable zlib compression support]),
            ac_zlib=$withval, ac_zlib=yes)
AC_MSG_CHECKING([whether to include zlib compression support])
if test x$ac_zlib != xno; then
 AC_MSG_RESULT(yes)
 AC_LIB_HAVE_LINKFLAGS(z,, [#include <zlib.h>], [compress (0, 0, 0, 0);])
 if test "$ac_cv_libz" != yes; then
   AC_MSG_WARN(
*** 
*** ZLIB was not found. You will not be able to use ZLIB compression.)
 fi
else
 AC_MSG_RESULT(no)
fi

if test x$ac_zlib != xno; then
  if test x$GNUTLS_REQUIRES_PRIVATE = x; then
    GNUTLS_REQUIRES_PRIVATE="Requires.private: zlib"
  else
    GNUTLS_REQUIRES_PRIVATE="$GNUTLS_REQUIRES_PRIVATE , zlib"
  fi
fi
AC_SUBST(GNUTLS_REQUIRES_PRIVATE)

lgl_INIT

AC_CHECK_FUNCS(getrusage,,)
AC_LIB_HAVE_LINKFLAGS(pthread,, [#include <pthread.h>], [pthread_mutex_lock (0);])
dnl for pakchois
AC_LIB_HAVE_LINKFLAGS(dl,, [#include <dlfcn.h>], [dlclose (0);])

LIBGNUTLS_LIBS="-L${libdir} -lgnutls $LIBS"
LIBGNUTLS_CFLAGS="-I${includedir}"
AC_SUBST(LIBGNUTLS_LIBS)
AC_SUBST(LIBGNUTLS_CFLAGS)

# Finish things from ../configure.ac.
AC_SUBST([WERROR_CFLAGS])
AC_SUBST([WSTACK_CFLAGS])
AC_SUBST([WARN_CFLAGS])
AC_PROG_CXX
AM_CONDITIONAL(ENABLE_CXX, test "$use_cxx" != "no")

AC_DEFINE([GNUTLS_INTERNAL_BUILD], 1, [We allow temporarily usage of deprecated functions - until they are removed.])

AC_CONFIG_FILES([
  Makefile
  gnutls.pc
  gl/Makefile
  gl/tests/Makefile
  includes/Makefile
  includes/gnutls/gnutls.h
  minitasn1/Makefile
  opencdk/Makefile
  openpgp/Makefile
  po/Makefile.in
  x509/Makefile
  gcrypt/Makefile
  nettle/Makefile
])
AC_OUTPUT
