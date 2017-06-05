dnl Process this file with autoconf to produce a configure script.
AC_INIT([wv],[1.2.9],[wvware-devel@sourceforge.net])

dnl This next section is courtesy gtk+
dnl
# Making releases:
#   WV_MICRO_VERSION += 1;
#   WV_INTERFACE_AGE += 1;
#   WV_BINARY_AGE += 1;
# if any functions have been added, set WV_INTERFACE_AGE to 0.
# if backwards compatibility has been broken,
# set WV_BINARY_AGE and WV_INTERFACE_AGE to 0.
#
WV_MAJOR_VERSION=1
WV_MINOR_VERSION=2
WV_MICRO_VERSION=9
WV_INTERFACE_AGE=5
WV_BINARY_AGE=5
WV_VERSION=$WV_MAJOR_VERSION.$WV_MINOR_VERSION.$WV_MICRO_VERSION



AC_CANONICAL_HOST
AC_CONFIG_SRCDIR([wvWare.c])


AC_SUBST(WV_MAJOR_VERSION)
AC_SUBST(WV_MINOR_VERSION)
AC_SUBST(WV_MICRO_VERSION)
AC_SUBST(WV_INTERFACE_AGE)
AC_SUBST(WV_BINARY_AGE)
AC_SUBST(WV_VERSION)

# libtool versioning
LT_RELEASE=$WV_MAJOR_VERSION.$WV_MINOR_VERSION
LT_CURRENT=`expr $WV_MICRO_VERSION - $WV_INTERFACE_AGE`
LT_REVISION=$WV_INTERFACE_AGE
LT_AGE=`expr $WV_BINARY_AGE - $WV_INTERFACE_AGE`
AC_SUBST(LT_RELEASE)
AC_SUBST(LT_CURRENT)
AC_SUBST(LT_REVISION)
AC_SUBST(LT_AGE)

# For automake.
VERSION=$WV_VERSION
PACKAGE=wv

dnl Initialize automake stuff
AM_INIT_AUTOMAKE($PACKAGE, $VERSION)

# Specify a configuration file
AM_CONFIG_HEADER(config.h)

AH_TOP([
#ifndef HAVE_WV_CONFIG_H
#define HAVE_WV_CONFIG_H
])

dnl Checks for programs.

AC_PROG_CC
if test "x$GCC" = "xyes"; then
	ANSI_CFLAGS="-ansi"
else
	ANSI_CFLAGS=""
fi
AC_SUBST(ANSI_CFLAGS)

AC_PROG_CPP
AC_PROG_AWK
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET

AC_PROG_LIBTOOL

AC_PATH_PROG(PKG_CONFIG, pkg-config, no)

# libgsf depends on glib and libxml2, so we should be fine
libgsf_req='libgsf-1 >= 1.13.0'
DEPS_PKGS="$libgsf_req"

# required, pkg-config'd packages
PKG_CHECK_MODULES(DEPS, [ $DEPS_PKGS ],,[AC_MSG_ERROR([$DEPS_PKG_ERRORS])])
AC_SUBST(DEPS_PKGS)
AC_SUBST(DEPS_CFLAGS)
AC_SUBST(DEPS_LIBS)

# optional deps
OPT_CFLAGS=
OPT_LIBS=

# these conditionals are still used, albeit libxml2 seems to be unconditionally required
AC_DEFINE(HAVE_LIBXML2,1,[Define if you have libxml2])
AC_SUBST(HAVE_LIBXML2)

dnl Checks for headers.

AC_HEADER_STDC
dnl AC_HEADER_SYS_WAIT
dnl AC_TYPE_SIGNAL
dnl AC_FUNC_WAIT3

AC_CHECK_HEADERS(fcntl.h io.h malloc.h sys/file.h sys/ioctl.h unistd.h)

case ${host_os} in
  *mingw*)
    link_win32_dll="yes"
    ;;
  *)
    link_win32_dll="no"
    ;;
esac
if test "$cross_compiling" = "yes"; then
  dllwrap="dllwrap"
else
  dllwrap=${host}-"dllwrap"
fi
AC_PATH_PROG(DLLWRAP, $dllwrap)

dnl Checks for typedefs, structures, and compiler characteristics.

AC_C_CONST

dnl zlib support
dnl =======================================================

_cppflags=$CPPFLAGS
_ldflags=$LDFLAGS

AC_ARG_WITH(zlib,[  --with-zlib=DIR         use libz in DIR],[
	if test "x$withval" = "xyes"; then
		ZLIB_DIR=""
	elif test "x$withval" != "xno"; then
		ZLIB_DIR="$withval"
		CPPFLAGS="$CPPFLAGS -I$withval/include"
		LDFLAGS="$LDFLAGS -L$withval/lib"
	else
		AC_MSG_WARN([No zlib? Are you sure? I think I'll look for it anyway...])
		ZLIB_DIR=""
	fi
],[	ZLIB_DIR=""
])

AC_CHECK_HEADER(zlib.h,[
	AC_CHECK_LIB(z,gzread,[
		AC_DEFINE(HAVE_ZLIB,1,[Define if you want zlib to uncompress wmf files])
		HAVE_ZLIB=1
		if test "x$ZLIB_DIR" != "x"; then
			ZLIB_CFLAGS="-I$ZLIB_DIR/include"
			ZLIB_LIBS="-L$ZLIB_DIR/lib -lz"
		else
			ZLIB_CFLAGS=""
			ZLIB_LIBS="-lz"
		fi
	],[	AC_CHECK_LIB(gz,gzread,[
			AC_DEFINE(HAVE_ZLIB,1,[Define if you want zlib to uncompress wmf files])
			HAVE_ZLIB=1
			if test "x$ZLIB_DIR" != "x"; then
				ZLIB_CFLAGS="-I$ZLIB_DIR/include"
				ZLIB_LIBS="-L$ZLIB_DIR/lib -lgz"
			else
				ZLIB_CFLAGS=""
				ZLIB_LIBS="-lgz"
			fi
		],[	dnl Shouldn't this be an error?
			AC_MSG_WARN([* * * Unable to find zlib, which is kinda necessary. * * *])
			HAVE_ZLIB=0
			ZLIB_CFLAGS=""
			ZLIB_LIBS=""
		])
	])
])

OPT_CFLAGS="$OPT_CFLAGS $ZLIB_CFLAGS"
OPT_LIBS="$OPT_LIBS $ZLIB_LIBS"

CPPFLAGS=$_cppflags
LDFLAGS=$_ldflags

dnl system png support
dnl ========================================================

_cppflags=$CPPFLAGS
_ldflags=$LDFLAGS

dnl libpng requires zlib
CPPFLAGS="$CPPFLAGS $ZLIB_CFLAGS"

AC_ARG_WITH(png,[  --with-png=DIR          use png library in DIR],[
	if test "x$withval" = "xyes"; then
		PNG_DIR=""
	elif test "x$withval" != "xno"; then
		PNG_DIR=$withval
		CPPFLAGS="$CPPFLAGS -I$withval/include"
		LDFLAGS="$LDFLAGS -L$withval/lib"
	else
		AC_MSG_WARN([No libpng? Are you sure? I think I'll look for it anyway...])
		PNG_DIR=""
	fi
],[	PNG_DIR=""
])

AC_CHECK_HEADER(png.h,[
	AC_CHECK_HEADER(pngconf.h,[
		AC_CHECK_LIB(png, png_free,[
			AC_DEFINE(HasPNG,1,[define if you have libpng])
			HAVE_PNG=1
			if test "x$PNG_DIR" != "x"; then
				PNG_CFLAGS="-I$PNG_DIR/include"
				PNG_LIBS="-L$PNG_DIR/lib -lpng"
			else
				PNG_CFLAGS=""
				PNG_LIBS="-lpng"
			fi
		],[	dnl Shouldn't this be an error?
			AC_MSG_WARN([* * * Unable to find libpng, which is kinda necessary. * * *])
			HAVE_PNG=0
			PNG_CFLAGS=""
			PNG_LIBS=""
		],$ZLIB_LIBS -lm)
	],[	dnl Shouldn't this be an error?
		AC_MSG_WARN([* * * Missing "pngconf.h" header file * * *])
		HAVE_PNG=0
		PNG_CFLAGS=""
		PNG_LIBS=""
	],[#if HAVE_ZLIB
#include <zlib.h>
#endif
	])
],[	dnl Shouldn't this be an error?
	AC_MSG_WARN([* * * Missing "png.h" header file * * *])
	HAVE_PNG=0
	PNG_CFLAGS=""
	PNG_LIBS=""
])

OPT_CFLAGS="$OPT_CFLAGS $PNG_CFLAGS"
OPT_LIBS="$OPT_LIBS $PNG_LIBS"

CPPFLAGS=$_cppflags
LDFLAGS=$_ldflags

dnl system libwmf support
dnl ========================================================

dnl Check for libwmf-0.2.1 or higher

_cppflags=$CPPFLAGS
_ldflags=$LDFLAGS

have_libwmf=no

AC_ARG_WITH(libwmf,[  --with-libwmf=DIR       use libwmf(2) in DIR],[
	if [ test "x$withval" != "xno" ]; then
		if [ test "x$withval" != "xyes" ]; then
			LIBWMF_DIR=$withval
		fi
		search_for_libwmf=yes
	else
		search_for_libwmf=no
	fi
],[	search_for_libwmf=no
])

if [ test $search_for_libwmf != no ]; then
	if [ test -n "$LIBWMF_DIR" ]; then
		AC_PATH_PROG(LIBWMF_CONFIG,libwmf-config, ,[$LIBWMF_DIR/bin:$PATH])
	else
		AC_PATH_PROG(LIBWMF_CONFIG,libwmf-config)
	fi

	if [ test -n "$LIBWMF_CONFIG" ]; then
		libwmf_cflags=`$LIBWMF_CONFIG --cflags`
		libwmf_libs=`$LIBWMF_CONFIG --libs`
	else
		AC_MSG_ERROR(* * * unable to find libwmf-config; unable to continue * * *)
	fi

	CPPFLAGS="$CPPFLAGS $libwmf_cflags"
	LDFLAGS="$LDFLAGS $libwmf_libs"

	AC_CHECK_HEADER(libwmf/api.h,[
		AC_CHECK_LIB(wmf,wmf_stream_create,have_libwmf=yes,[
			AC_MSG_ERROR(* * * libwmf >= 0.2.1 required * * *)
		],$libwmf_libs)
		AC_CHECK_HEADER(libwmf/foreign.h,[
			AC_DEFINE(HAVE_LIBWMF_FOREIGN_H,1,[Define if libwmf(2) has support for reading 'foreign' image formats])
		],[	AC_MSG_WARN(* * * libwmf >= 0.2.2 recommended * * *)
		])
	])

	if [ test $have_libwmf != no]; then
		AC_DEFINE(HAVE_LIBWMF,1,[Define if you have libwmf(2) and want it to convert wmf to png files])

		WMF_CFLAGS="$libwmf_cflags"
		WMF_LIBS="$libwmf_libs"
	fi
fi

CPPFLAGS=$_cppflags
LDFLAGS=$_ldflags

OPT_CFLAGS="$OPT_CFLAGS $WMF_CFLAGS"
OPT_LIBS="$OPT_LIBS $WMF_LIBS"

AC_SUBST(OPT_CFLAGS)
AC_SUBST(OPT_LIBS)

AC_LANG_C

CPPFLAGS=$_cppflags
LDFLAGS=$_ldflags

dnl ========================================================
dnl begin of possible optimization tests
dnl read notes/optimization

AC_CHECK_SIZEOF(char,1)
AC_CHECK_SIZEOF(short,2)
AC_CHECK_SIZEOF(int,4)
AC_CHECK_SIZEOF(long,4)

if [test "$SIZEOF_CHAR" != "1" ]; then
    if [test "$SIZEOF_SHORT" != "2" ]; then
        if [test "$SIZEOF_INT" != "4" ]; then
            AC_DEFINE(MATCHED_TYPE,1,[Define if sizeof({char,short,int}) != {1,2,4}])
        fi
    fi
fi

if [ test "$cross_compiling" != "yes" ]; then
    AC_C_BIGENDIAN
dnl	here we will know the endianness
	if [ test "$ac_cv_c_bigendian" = "no" ]; then
	    AC_DEFINE(XML_BYTE_ORDER,12,[Define as 12 for little-endian, 21 for big-endian])
	else 
		if [ test "$ac_cv_c_bigendian" = "yes" ]; then
			AC_DEFINE(XML_BYTE_ORDER,21,[Define as 12 for little-endian, 21 for big-endian])
		fi
	fi
else
dnl	here we will not know the endianness, so we will
dnl go with the mechanism that works everywhere
    AC_DEFINE(WORDS_BIGENDIAN,1,[Define if byte order is big-endian])
fi

dnl given matched types and little endian we can read types larger
dnl than a byte with fread and avoid byteswapping

dnl at some later stage we will have a test to see if the compiler can
dnl get rid of holes, for the moment we will assume only that we can
dnl do it if we have __GNUC__ defined

dnl end of possible optimization tests

dnl check for getopt
dnl 
AC_CHECK_FUNC(getopt_long,,[
	AC_LIBOBJ(getopt1)
	AC_LIBOBJ(getopt)
])

dnl check for strcasecmp
dnl 
AC_CHECK_FUNC(strcasecmp,,[
	AC_LIBOBJ(strcasecmp)
])

AC_EGREP_HEADER(strcasecmp,string.h,,[
	AC_DEFINE(DEFINE_STRCASECMP,1,[Define this if strcasecmp must be defined])
])

dnl check for memcpy, if not there we'll use bcopy
dnl 
AC_CHECK_FUNCS(memcpy)

dnl check for mmap
dnl 
AC_FUNC_MMAP

AH_BOTTOM([
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
#if defined(HAVE_ERRNO_H)
#include <errno.h>
#endif
#include <sys/types.h>
#include <sys/stat.h>
#include <limits.h>
#include <stdarg.h>

#if defined(__GNUC__) && !defined(WORDS_BIGENDIAN) && defined(MATCHED_TYPE)
#define NO_HOLES
#endif

#ifndef HAVE_MEMCPY
#define memcpy(d, s, n) bcopy ((s), (d), (n))
#endif /* not HAVE_MEMCPY */

/* redefs of things that are either in glibc or we have to include them ourselves */
#if defined(WIN32) && !defined(__MWERKS__) && !defined(__WINE__)
#define strcasecmp(s1,s2) stricmp(s1,s2)
#else
#if !defined(__GLIBC__) || (__GLIBC__ < 2) || (__GLIBC__ == 2 && __GLIBC_MINOR__ < 2)
#ifdef DEFINE_STRCASECMP
int strcasecmp (const char *s1, const char *s2);
#endif
#endif
#endif

#endif /* ! HAVE_WV_CONFIG_H */
])

AC_CONFIG_FILES([
	wvAbw
	wvDVI
	wvPS
	wvPDF
	wvHtml
	wvDocBook
	wvLatex
	wvCleanLatex
	wvText
	wvWml
	wv-1.0.pc
	version.c
	GNUmakefile
	xml/GNUmakefile
	help/GNUmakefile
	help/man/GNUmakefile
	patterns/GNUmakefile
	wingdingfont/GNUmakefile
])
AC_OUTPUT
