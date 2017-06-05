# gnupg.m4 - autoconf macros as used by GnuPG.
# Copyright (C) 1998, 1999, 2000, 2001, 2003 Free Software Foundation, Inc.
#
# This file is free software; as a special exception the author gives
# unlimited permission to copy and/or distribute it, with or without
# modifications, as long as this notice is preserved.
#
# This file is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY, to the extent permitted by law; without even the
# implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.



dnl Stolen from gcc
dnl Define MKDIR_TAKES_ONE_ARG if mkdir accepts only one argument instead
dnl of the usual 2.
AC_DEFUN([GNUPG_FUNC_MKDIR_TAKES_ONE_ARG],
[AC_CHECK_HEADERS(sys/stat.h unistd.h direct.h)
AC_CACHE_CHECK([if mkdir takes one argument], gnupg_cv_mkdir_takes_one_arg,
[AC_TRY_COMPILE([
#include <sys/types.h>
#ifdef HAVE_SYS_STAT_H
# include <sys/stat.h>
#endif
#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif
#ifdef HAVE_DIRECT_H
# include <direct.h>
#endif], [mkdir ("foo", 0);],
        gnupg_cv_mkdir_takes_one_arg=no, gnupg_cv_mkdir_takes_one_arg=yes)])
if test $gnupg_cv_mkdir_takes_one_arg = yes ; then
  AC_DEFINE(MKDIR_TAKES_ONE_ARG,1,
            [Defined if mkdir() does not take permission flags])
fi
])

