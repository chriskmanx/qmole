dnl Process this file with autoconf to produce a configure script.
dnl This file is part of GNU cpio
dnl Copyright (C) 2003, 2004, 2005, 2006, 2007, 2009, 2010 Free Software
dnl Foundation, Inc.
dnl
dnl This program is free software; you can redistribute it and/or modify
dnl it under the terms of the GNU General Public License as published by
dnl the Free Software Foundation; either version 3, or (at your option)
dnl any later version.
dnl
dnl This program is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl GNU General Public License for more details.
dnl
dnl You should have received a copy of the GNU General Public License
dnl along with this program; if not, write to the Free Software Foundation,
dnl Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

AC_INIT([GNU cpio], [2.11], [bug-cpio@gnu.org])
AC_CONFIG_SRCDIR(src/cpio.h)
AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_HEADERS([config.h])
AC_PREREQ([2.63])
AM_INIT_AUTOMAKE([1.11.1 gnits tar-ustar dist-bzip2 dist-shar std-options silent-rules])

# Enable silent rules by default:
AM_SILENT_RULES([yes])

dnl Check for programs
AC_PROG_CC
AC_PROG_CPP
AC_PROG_GCC_TRADITIONAL
AC_PROG_INSTALL
AC_PROG_RANLIB
AC_PROG_YACC
gl_EARLY
AC_C_PROTOTYPES

AC_SYS_LARGEFILE

AC_TYPE_SIGNAL
AC_HEADER_MAJOR
AC_C_CONST
AC_TYPE_UID_T
AC_CHECK_TYPE(gid_t, int)
AC_HEADER_STDC
AC_HEADER_DIRENT

AC_CHECK_FUNCS([fchmod fchown])
# This is needed for mingw build
AC_CHECK_FUNCS([setmode getpwuid getpwnam getgrgid getgrnam pipe fork getuid geteuid])

# gnulib modules
gl_INIT
# paxutils modules
cpio_PAXUTILS
# Additional system dependencies (for mingw)
CPIO_SYSDEP
CPIO_PACKED_STRUCTS
AC_SUBST(CPIO_MT_PROG)

AC_ARG_ENABLE(mt,
 AC_HELP_STRING([--enable-mt], [Enable building of mt program]),
 [case $enableval in
  yes) CPIO_MT_PROG='mt$(EXEEXT)';;
  no)  ;;
  esac])

AC_CHECK_HEADERS([unistd.h stdlib.h string.h fcntl.h pwd.h grp.h sys/io/trioctl.h utmp.h getopt.h locale.h libintl.h sys/wait.h utime.h locale.h process.h sys/ioctl.h])

AC_CHECK_DECLS([errno, getpwnam, getgrnam, getgrgid, strdup, strerror, getenv, atoi, exit], , , [
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#ifdef HAVE_STRING_H
# include <string.h>
#else
# include <strings.h>
#endif
#include <pwd.h>
#include <grp.h>])

# Gettext.
AM_ICONV
AM_GNU_GETTEXT([external], [need-formatstring-macros])
AM_GNU_GETTEXT_VERSION(0.17)

LIBS="$LIBS $LIB_CLOCK_GETTIME"

# Initialize the test suite.
AC_CONFIG_TESTDIR(tests)
AC_CONFIG_FILES([tests/Makefile tests/atlocal]) # FIXME: tests/preset?
AM_MISSING_PROG([AUTOM4TE], [autom4te])

AC_CONFIG_FILES([Makefile
 doc/Makefile
 headers/Makefile
 gnu/Makefile
 lib/Makefile
 rmt/Makefile
 src/Makefile
 po/Makefile.in])
AC_OUTPUT
