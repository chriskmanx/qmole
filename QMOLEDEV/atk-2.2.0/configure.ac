dnl Process this file with autoconf to produce a configure script.

AC_PREREQ([2.63])

dnl ==========================================================================
dnl                              Versioning              
dnl ==========================================================================

dnl Making releases:
dnl   ATK_MICRO_VERSION += 1;
dnl   ATK_INTERFACE_AGE += 1;
dnl   ATK_BINARY_AGE += 1;
dnl if any functions have been added, set ATK_INTERFACE_AGE to 0.
dnl if backwards compatibility has been broken,
dnl set ATK_BINARY_AGE _and_ ATK_INTERFACE_AGE to 0.

dnl The triplet 
m4_define([atk_major_version], [2])
m4_define([atk_minor_version], [2])
m4_define([atk_micro_version], [0])
m4_define([atk_version],
          [atk_major_version.atk_minor_version.atk_micro_version])

dnl The X.Y in -latk-X.Y line. This is expected to stay 1.0 until Atk 3.
m4_define([atk_api_version], [1.0])

dnl Number of releases since we've added interfaces
m4_define([atk_interface_age], [1])

dnl binary_age includes major version as ATK 2 is still fully API and ABI compatible
m4_define([atk_binary_age],
          [m4_eval(10000 * atk_major_version + 100 * atk_minor_version + 10 + atk_micro_version)])

m4_define([lt_current],
          [m4_eval(10000 * atk_major_version + 100 * atk_minor_version + 10 + atk_micro_version - atk_interface_age)])
m4_define([lt_revision], [atk_interface_age])
m4_define([lt_age], [m4_eval(atk_binary_age - atk_interface_age)])
m4_define([lt_version_info], [lt_current:lt_revision:lt_age])
m4_define([lt_current_minus_age], [m4_eval(lt_current - lt_age)])

AC_INIT([atk],
        [atk_version],
        [http://bugzilla.gnome.org/enter_bug.cgi?product=atk],
        [atk])

AC_CONFIG_HEADERS([config.h])
AC_CONFIG_SRCDIR([ChangeLog])
AC_CONFIG_MACRO_DIR([m4])

AM_INIT_AUTOMAKE([1.10 -Wno-portability])

# Support silent build rules, requires at least automake-1.11. Disable
# by either passing --disable-silent-rules to configure or passing V=1
# to make
m4_ifdef([AM_SILENT_RULES],[AM_SILENT_RULES([yes])])

dnl ==========================================================================
dnl
dnl If you add a version number here, you *must* add an AC_SUBST line for
dnl it too, or it will never make it into the spec file!
dnl
dnl ==========================================================================

ATK_MAJOR_VERSION=atk_major_version
ATK_MINOR_VERSION=atk_minor_version
ATK_MICRO_VERSION=atk_micro_version
ATK_VERSION=atk_version
ATK_API_VERSION=atk_api_version
ATK_INTERFACE_AGE=atk_interface_age
ATK_BINARY_AGE=atk_binary_age

AC_SUBST(ATK_MAJOR_VERSION)
AC_SUBST(ATK_MINOR_VERSION)
AC_SUBST(ATK_VERSION)
AC_SUBST(ATK_API_VERSION)
AC_SUBST(ATK_MICRO_VERSION)
AC_SUBST(ATK_INTERFACE_AGE)
AC_SUBST(ATK_BINARY_AGE)

dnl libtool versioning
LT_VERSION_INFO=lt_version_info
LT_CURRENT_MINUS_AGE=lt_current_minus_age
AC_SUBST(LT_VERSION_INFO)
AC_SUBST(LT_CURRENT_MINUS_AGE)

dnl ==========================================================================

# Check for programs
AC_PROG_CC

# Initialize libtool
LT_PREREQ([2.2])
LT_INIT([disable-static win32-dll])

AC_CHECK_FUNCS(bind_textdomain_codeset)

AC_MSG_CHECKING([for some Win32 platform])
case "$host" in
  *-*-mingw*|*-*-cygwin*)
    platform_win32=yes
    ;;
  *)
    platform_win32=no
    ;;
esac
AC_MSG_RESULT([$platform_win32])
AM_CONDITIONAL(PLATFORM_WIN32, test "$platform_win32" = "yes")

AC_MSG_CHECKING([for native Win32 platform])
case "$host" in
  *-*-mingw*)
    atk_native_win32=yes
    case "$host" in
      x86_64-*-*)
	LIB_EXE_MACHINE_FLAG=X64
	;;
      *)
	LIB_EXE_MACHINE_FLAG=X86
	;;
    esac
    ;;
  *)
    atk_native_win32=no
    ;;
esac
AC_MSG_RESULT([$atk_native_win32])
AM_CONDITIONAL(OS_WIN32, test "$atk_native_win32" = "yes")

AC_SUBST(LIB_EXE_MACHINE_FLAG)

if test "$atk_native_win32" = "yes"; then
  AC_CHECK_TOOL(WINDRES, windres, no)
  if test "$WINDRES" = no; then
    AC_MSG_ERROR([*** Could not find an implementation of windres in your PATH.])
  fi
  AC_CHECK_PROG(ms_librarian, lib.exe, yes, no)

fi
AM_CONDITIONAL(MS_LIB_AVAILABLE, test x$ms_librarian = xyes)

dnl Cache $ACLOCAL_FLAGS
AC_CACHE_CHECK([for aclocal flags], ac_cv_atk_aclocal_flags,[
   ac_cv_atk_aclocal_flags="$ACLOCAL_FLAGS"
])
ACLOCAL="$ACLOCAL $ac_cv_atk_aclocal_flags"

AC_ARG_ENABLE(rebuilds,
              [AS_HELP_STRING([--disable-rebuilds],
                              [disable all source autogeneration rules])],,
              [enable_rebuilds=yes])

CPPFLAGS="$CPPFLAGS -DG_DISABLE_SINGLE_INCLUDES -DATK_DISABLE_SINGLE_INCLUDES"

changequote(,)dnl
if test "x$GCC" = "xyes"; then
  case " $CFLAGS " in
  *[\ \ ]-Wall[\ \	]*) ;;
  *) CFLAGS="$CFLAGS -Wall" ;;
  esac
fi
changequote([,])dnl

GLIB_REQUIRED_VERSION=2.0.0
GLIB_PACKAGES="gobject-2.0"
AC_SUBST(GLIB_PACKAGES)
AC_SUBST(GLIB_REQUIRED_VERSION)

dnl This PATH_GLIB is somewhat redundant, but does a sanity compile and 
dnl importantly defines the GLIB_GENMARSHAL variable for subst into the
dnl Makefile
AM_PATH_GLIB_2_0($GLIB_REQUIRED_VERSION, :,
  AC_MSG_ERROR([
*** GLIB $GLIB_REQUIRED_VERSION or better is required. The latest version of 
*** GLIB is always available from ftp://ftp.gtk.org/. If GLIB is installed
*** but not in the same location as pkg-config add the location of the file 
*** glib-2.0.pc to the environment variable PKG_CONFIG_PATH.]),
  gobject)

PKG_CHECK_MODULES(DEP, glib-2.0 >= $GLIB_REQUIRED_VERSION $GLIB_PACKAGES, ,
  AC_MSG_ERROR([
        *** GLib not found. You can find it on ftp://ftp.gtk.org
        *** Errors follow:
            $DEP_PKG_ERRORS]))

# Rerun PKG_CONFIG to add gthread-2.0 cflags, but not libs
DEP_CFLAGS=`$PKG_CONFIG --cflags $GLIB_PACKAGES gthread-2.0`

# i18n stuff
GETTEXT_PACKAGE=atk10
AC_SUBST(GETTEXT_PACKAGE)
AC_DEFINE_UNQUOTED(GETTEXT_PACKAGE, ["$GETTEXT_PACKAGE"],
  [Define the gettext package to be used])

ALL_LINGUAS="`grep -v '^#' "$srcdir/po/LINGUAS" | tr '\n' ' '`"
AM_GLIB_GNU_GETTEXT

# Introspection support
GOBJECT_INTROSPECTION_CHECK([0.6.7])

# Documentation support
GTK_DOC_CHECK([1.13])

# define a MAINT-like variable REBUILD which is set if Perl
# and awk are found, so autogenerated sources can be rebuilt

AC_PROG_AWK
AC_CHECK_PROGS(PERL, perl5 perl)

REBUILD=\#
if test "x$enable_rebuilds" = "xyes" && \
	test -n "$PERL" && \
	$PERL -e 'exit !($] >= 5.002)' > /dev/null 2>&1 && \
     test -n "$AWK" ; then
  REBUILD=
fi
AC_SUBST(REBUILD)

AC_CONFIG_FILES([
Makefile
po/Makefile.in
atk.pc
atk-uninstalled.pc
atk/Makefile
atk/atk.rc
tests/Makefile
build/Makefile
build/win32/Makefile
build/win32/vs9/Makefile
build/win32/vs10/Makefile
docs/Makefile
docs/version.xml
atk.spec
atk-zip.sh
config.h.win32
])

AC_OUTPUT
