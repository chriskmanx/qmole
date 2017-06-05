dnl =========================================================================
dnl =                                                                       =
dnl =                         Applied Type System                           =
dnl =                                                                       =
dnl =                              Hongwei Xi                               =
dnl =                                                                       =
dnl =========================================================================

dnl =
dnl =  ATS - Unleashing the Power of Types!
dnl =
dnl =  Copyright (C) 2002-2010 Hongwei Xi.
dnl =
dnl =  ATS is  free software;  you can redistribute it and/or modify it under
dnl =  the  terms of the  GNU General Public License as published by the Free
dnl =  Software Foundation; either version 2.1, or (at your option) any later
dnl =  version.
dnl = 
dnl =  ATS is distributed in the hope that it will be useful, but WITHOUT ANY
dnl =  WARRANTY; without  even  the  implied  warranty  of MERCHANTABILITY or
dnl =  FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public License
dnl =  for more details.
dnl = 
dnl =  You  should  have  received  a  copy of the GNU General Public License
dnl =  along  with  ATS;  see  the  file  COPYING.  If not, write to the Free
dnl =  Software Foundation, 51  Franklin  Street,  Fifth  Floor,  Boston,  MA
dnl =  02110-1301, USA.
dnl =

dnl =
dnl = Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
dnl = Author: Likai Liu (liulk AT cs DOT bu DOT edu)
dnl =

dnl = beg of [configure.ac]

dnl = Autoconf requirements
dnl = AC_INIT(package, version, bug-report-address)
dnl = information on the package
dnl = checks for programs
dnl = checks for libraries
dnl = checks for header files
dnl = checks for types
dnl = checks for structures
dnl = checks for compiler characteristics
dnl = checks for library functions
dnl = checks for system services
dnl = AC_CONFIG_FILES([file...])

dnl = AC_INIT([ATS/Anairiats], [0.1.8], [hwxi@cs.bu.edu])
dnl = AC_INIT([ATS/Anairiats], [0.1.9], [hwxi@cs.bu.edu])
dnl = AC_INIT([ATS/Anairiats], [0.2.0], [hwxi@cs.bu.edu])
dnl = AC_INIT([ATS/Anairiats], [0.2.1], [hwxi@cs.bu.edu])
dnl = AC_INIT([ATS/Anairiats], [0.2.2], [hwxi@cs.bu.edu])
dnl = AC_INIT([ATS/Anairiats], [0.2.3], [hwxi@cs.bu.edu])
dnl = AC_INIT([ATS/Anairiats], [0.2.4], [hwxi@cs.bu.edu])
dnl = AC_INIT([ATS/Anairiats], [0.2.5], [hwxi@cs.bu.edu])
AC_INIT([ATS/Anairiats], [0.2.6], [hwxi@cs.bu.edu])
AC_CONFIG_SRCDIR([config.h.in])
AC_CONFIG_HEADER([config.h])
AC_CONFIG_FILES([config.mk])
AC_CONFIG_FILES([ats_env.sh])
AC_CONFIG_FILES([test.sh])

AM_INIT_AUTOMAKE

AC_PROG_CC
AC_PROG_INSTALL
AC_PROG_MKDIR_P
AC_PROG_LN_S
PKG_PROG_PKG_CONFIG

dnl = check for complex.h
AC_CHECK_HEADERS(complex.h)

dnl = check for curses.h
AC_CHECK_HEADERS(curses.h)

dnl = check for gmp.h
AC_CHECK_HEADERS(gmp.h)
AC_SUBST(HAVE_LIBGMP, ${ac_cv_header_gmp_h%no})

dnl = check for pcre.h
AC_CHECK_HEADERS(pcre.h)
AC_SUBST(HAVE_LIBPCRE, ${ac_cv_header_pcre_h%no})

dnl = check for [cairo/cairo]
dnl = AC_CHECK_HEADERS(cairo/cairo.h)

dnl = check for [glib-2.0.0]
AM_PATH_GLIB_2_0(2.10.0, [
  AC_DEFINE([HAVE_LIBGLIB20], [1], [Define to 1 if you have glib-2.0])
  have_glib=1
])
AC_SUBST(HAVE_LIBGLIB20, $have_glib)

dnl = check for [gtk+-2.0.0]
AM_PATH_GTK_2_0(2.10.0, [
  AC_DEFINE([HAVE_LIBGTK20], [1], [Define to 1 if you have gtk+2.0])
  have_gtk=1
])
AC_SUBST(HAVE_LIBGTK20, $have_gtk)

dnl = check for [GL/gl.h GL/glut.h]
dnl = AC_CHECK_HEADERS(GL/gl.h GL/glut.h)

dnl = check for [SDL]
dnl = AM_PATH_SDL(1.0.0, [
dnl =  AC_DEFINE([HAVE_LIBSDL10], [1], [Define to 1 if you have SDL-1.0])
dnl =  have_sdl=1
dnl = ])
dnl = AC_SUBST(HAVE_LIBSDL10, $have_sdl)

dnl = check for SIZEOF_VOIDP
AC_CHECK_SIZEOF(void*)

dnl = check for some functions
AC_CHECK_FUNCS(posix_memalign memalign)

AH_TEMPLATE([HAVE_GCBDW],
	    [Informs us the availability of Boehm-Demers-Weiser
	     garbage collection installed on the system.])
PKG_CHECK_MODULES([GCBDW], [Boehm-Demers-Weiser GC >= 7.1], [AC_DEFINE(HAVE_GCBDW)], [true])

CFLAGS="$CFLAGS $GCBDW_CFLAGS"
LDFLAGS="$LDFLAGS $GCBDW_LDFLAGS"

AC_OUTPUT

dnl = end of [configure.ac]
