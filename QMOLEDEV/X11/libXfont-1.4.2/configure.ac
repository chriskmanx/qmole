dnl 
dnl  $Id: configure.ac,v 1.12 2004/04/12 14:24:45 daniel Exp $
dnl 
dnl  Copyright © 2003 Keith Packard
dnl 
dnl  Permission to use, copy, modify, distribute, and sell this software and its
dnl  documentation for any purpose is hereby granted without fee, provided that
dnl  the above copyright notice appear in all copies and that both that
dnl  copyright notice and this permission notice appear in supporting
dnl  documentation, and that the name of Keith Packard not be used in
dnl  advertising or publicity pertaining to distribution of the software without
dnl  specific, written prior permission.  Keith Packard makes no
dnl  representations about the suitability of this software for any purpose.  It
dnl  is provided "as is" without express or implied warranty.
dnl 
dnl  KEITH PACKARD DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
dnl  INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO
dnl  EVENT SHALL KEITH PACKARD BE LIABLE FOR ANY SPECIAL, INDIRECT OR
dnl  CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
dnl  DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
dnl  TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
dnl  PERFORMANCE OF THIS SOFTWARE.
dnl
dnl Process this file with autoconf to create configure.

AC_PREREQ([2.57])

AC_INIT([libXfont],
	1.4.2,
	[keithp@keithp.com],
	libXfont)
dnl
dnl This is the package version number, not the shared library
dnl version.  This same version number must appear in Xfont.h
dnl Yes, it is a pain to synchronize version numbers.  Unfortunately, it's
dnl not possible to extract the version number here from Xfont.h
dnl
AM_INIT_AUTOMAKE([dist-bzip2])
AM_CONFIG_HEADER(config.h)
AC_CONFIG_AUX_DIR(.)
AC_CONFIG_HEADERS([include/X11/fonts/fontconf.h])

# Check for progs
AC_PROG_CC
AC_PROG_LIBTOOL

# Look for headers
AC_CHECK_HEADERS([endian.h])

#
# select libraries to include
#

AC_ARG_ENABLE(freetype, [ --enable-freetype ],[XFONT_FREETYPE=$enableval],[XFONT_FREETYPE=no])
AM_CONDITIONAL(XFONT_FREETYPE, [test x$XFONT_FREETYPE = xyes])
if test x$XFONT_FREETYPE = xyes; then
	AC_DEFINE(XFONT_FREETYPE,1,[Support FreeType fonts])
	AC_ARG_WITH(freetype-config, [  --with-freetype-config=PROG   Use FreeType configuration program PROG], freetype_config=$withval, freetype_config=yes)
	
	if test "$freetype_config" = "yes"; then 
		AC_PATH_PROG(ft_config,freetype-config,no)
		if test "$ft_config" = "no"; then
			AC_MSG_ERROR([You must have freetype installed; see http://www.freetype.org/])
		fi
	else
		ft_config="$freetype_config"
	fi
	
	FREETYPE_CFLAGS="`$ft_config --cflags`"
	FREETYPE_LIBS="`$ft_config --libs`"
	
else
	FREETYPE_CFLAGS=""
	FREETYPE_LIBS=""
fi
AC_SUBST(FREETYPE_LIBS)
AC_SUBST(FREETYPE_CFLAGS)

AC_ARG_ENABLE(fc, [ --enable-fc ],[XFONT_FC=$enableval],[XFONT_FC=no])
AM_CONDITIONAL(XFONT_FC, [test x$XFONT_FC = xyes])
if test x$XFONT_FC = xyes; then
	AC_DEFINE(XFONT_FC,1,[Support the X Font Services Protocol])
fi

AC_ARG_ENABLE(builtins, [ --disable-builtins ], [XFONT_BUILTINS=$enableval], [XFONT_BUILTINS=yes])
AM_CONDITIONAL(XFONT_BUILTINS, [test "x$XFONT_BUILTINS" = xyes ])
if test "x$XFONT_BUILTINS" = xyes; then
	AC_DEFINE(XFONT_BUILTINS,1,[Support built-in fonts])
fi

AC_ARG_ENABLE(bitmap, [ --disable-bitmap ], [XFONT_BITMAP=$enableval], [XFONT_BITMAP=yes])
AM_CONDITIONAL(XFONT_BITMAP, [test "x$XFONT_BITMAP" = xyes ])
if test "x$XFONT_BITMAP" = xyes; then
	AC_DEFINE(XFONT_BITMAP,1,[Support bitmap font files])
fi

if test "x$XFONT_BULITINS" = xyes -o "x$XFONT_BITMAP" = xyes -o "x$XFONT_FREETYPE" = xyes; then
	XFONT_FONTFILE=yes
else
	XFONT_FONTFILE=no
fi
AM_CONDITIONAL(XFONT_FONTFILE, [test "x$XFONT_FONTFILE" = xyes])
if test "x$XFONT_FONTFILE" = xyes; then
	AC_DEFINE(XFONT_FONTFILE,,[Support fonts in files])
fi

AC_CHECK_LIB(z, gzopen, [Z_LIBS=-lz
AC_SUBST(Z_LIBS)], AC_MSG_ERROR([*** zlib is required]))

AC_DEFINE(PCFFORMAT,,[Support PCF format fonts])
AC_DEFINE(X_GZIP_FONT_COMPRESSION,,[Support gzip fonts])

AC_CHECK_LIB(m, hypot, [MATH_LIBS=-lm
AC_SUBST(MATH_LIBS)], AC_MSG_ERROR([*** libm is required]))

PKG_CHECK_MODULES(XFONT, xproto)

if test "x$GCC" = "xyes"; then
	GCC_WARNINGS="-Wall -Wpointer-arith -Wstrict-prototypes \
	-Wmissing-prototypes -Wmissing-declarations \
	-Wnested-externs -fno-strict-aliasing"
	CFLAGS="$GCC_WARNINGS $CFLAGS"
fi

CFLAGS="$CFLAGS $XFONT_CFLAGS"
LIBS="$LIBS $XFONT_LIBS"

AC_OUTPUT([Makefile
	   fontfile/Makefile
	   FreeType/Makefile
	   builtins/Makefile
	   bitmap/Makefile
	   fc/Makefile
	   util/Makefile
	   stubs/Makefile
           xfont.pc])
