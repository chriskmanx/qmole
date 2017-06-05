-*- mode: m4 -*-
AC_PREREQ(2.52)

dnl the pygobject version number
m4_define(pygobject_major_version, 2)
m4_define(pygobject_minor_version, 16)
m4_define(pygobject_micro_version, 1)
m4_define(pygobject_version, pygobject_major_version.pygobject_minor_version.pygobject_micro_version)

dnl versions of packages we require ...
m4_define(glib_required_version, 2.14.0)
m4_define(gio_required_version, 2.16.0)
m4_define(giounix_required_version, 2.16.0)

AC_INIT(pygobject, pygobject_version,
        [http://bugzilla.gnome.org/enter_bug.cgi?product=pygobject])
AC_SUBST(ACLOCAL_AMFLAGS, "-I m4 -I .")
AC_CONFIG_MACRO_DIR([m4])

AC_DEFINE(PYGOBJECT_MAJOR_VERSION, pygobject_major_version, [pygobject major version])
AC_SUBST(PYGOBJECT_MAJOR_VERSION, pygobject_major_version)
AC_DEFINE(PYGOBJECT_MINOR_VERSION, pygobject_minor_version, [pygobject minor version])
AC_SUBST(PYGOBJECT_MINOR_VERSION, pygobject_minor_version)
AC_DEFINE(PYGOBJECT_MICRO_VERSION, pygobject_micro_version, [pygobject micro version])
AC_SUBST(PYGOBJECT_MICRO_VERSION, pygobject_micro_version)

AC_CONFIG_SRCDIR([gobject/gobjectmodule.c])
AM_CONFIG_HEADER(config.h)

AM_INIT_AUTOMAKE

dnl put the ACLOCAL flags in the makefile
ACLOCAL="$ACLOCAL $ACLOCAL_FLAGS"

AC_CANONICAL_BUILD
AC_CANONICAL_HOST
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

AC_MSG_CHECKING([for native Win32])
case "$host" in
  *-*-mingw*)
    os_win32=yes
    ;;
  *)
    os_win32=no
    ;;
esac
AC_MSG_RESULT([$os_win32])
AM_CONDITIONAL(OS_WIN32, test "$os_win32" = "yes")

AC_DISABLE_STATIC

dnl XXXX hack to kill off all the libtool tags ...
dnl it isn't like we are using C++ or Fortran.
dnl (copied from libglade/configure.in)
m4_define([_LT_AC_TAGCONFIG],[])

AC_LIBTOOL_WIN32_DLL
AC_PROG_LIBTOOL
dnl when using libtool 2.x create libtool early, because it's used in configure
m4_ifdef([LT_OUTPUT], [LT_OUTPUT]) 
AM_PROG_CC_C_O

JD_PATH_PYTHON(2.3.5)

JD_CHECK_PYTHON_HEADERS(,[AC_MSG_ERROR(could not find Python headers)])

AC_MSG_CHECKING([for PySignal_SetWakeupFd in Python.h])
py_prefix=`$PYTHON -c "import sys; sys.stdout.write(sys.prefix)"`
if test -x "$PYTHON-config"; then
PYTHON_INCLUDES=`$PYTHON-config --includes 2>/dev/null`
else
PYTHON_INCLUDES="-I${py_prefix}/include/python${PYTHON_VERSION}"
fi
old_CPPFLAGS=$CPPFLAGS
CPPFLAGS="-Wall -Werror $PYTHON_INCLUDES"
AC_TRY_COMPILE([#include <Python.h>],
  [PySignal_SetWakeupFd(0);],
  setwakeupfd_ok=yes,
  setwakeupfd_ok=no)
AC_MSG_RESULT($setwakeupfd_ok)
if test "x$setwakeupfd_ok" != xno; then
  AC_DEFINE(HAVE_PYSIGNAL_SETWAKEUPFD, 1,
                [Define to 1 if PySignal_SetWakeupFd is available])
fi
CPPFLAGS=$old_CPPFLAGS

PLATFORM=`$PYTHON -c "import sys; from distutils import util; sys.stdout.write(util.get_platform())"`
AC_SUBST(PLATFORM)

AC_ARG_ENABLE(thread,
  AC_HELP_STRING([--disable-thread], [Disable pygobject threading support]),,
  enable_thread=yes)

dnl Building documentation
AC_ARG_ENABLE(docs,
  AC_HELP_STRING([--enable-docs], [Enable documentation building]),enable_docs=$enableval,
  enable_docs=no)
if test "${enable_docs}" != no; then
  dnl
  dnl Check for xsltproc
  dnl
  AC_PATH_PROG([XSLTPROC], [xsltproc])
  if test -z "$XSLTPROC"; then
    enable_docs=no
  fi

  dnl check for DocBook DTD and stylesheets in the local catalog.
  dnl JH_CHECK_XML_CATALOG([-//OASIS//DTD DocBook XML V4.1.2//EN],
  dnl   [DocBook XML DTD V4.1.2],,enable_docs=no)
  dnl JH_CHECK_XML_CATALOG([http://docbook.sourceforge.net/release/xsl/current/manpages/docbook.xsl],
  dnl   [DocBook XSL Stylesheets],,enable_docs=no)
fi

AM_CONDITIONAL(ENABLE_DOCS, test x$enable_docs != xno)

AM_CHECK_PYMOD(thread,,,enable_thread=no)

AC_MSG_CHECKING(whether to enable threading in pygobject)
if test "x$enable_thread" != xno; then
  extra_mods=gthread
  THREADING_CFLAGS=
  AC_MSG_RESULT(yes)
else
  extra_mods=
  THREADING_CFLAGS="-DDISABLE_THREADING"
  AC_MSG_RESULT(no)
fi
AC_SUBST(THREADING_CFLAGS)
CPPFLAGS="${CPPFLAGS} $THREADING_CFLAGS"

dnl get rid of the -export-dynamic stuff from the configure flags ...
export_dynamic=`(./libtool --config; echo eval echo \\$export_dynamic_flag_spec) | sh`

dnl glib
AM_PATH_GLIB_2_0(glib_required_version,,[AC_MSG_ERROR(maybe you want the pygobject-2-4 branch?)],gobject $extra_mods)
if test -n "$export_dynamic"; then
  GLIB_LIBS=`echo $GLIB_LIBS | sed -e "s/$export_dynamic//"`
fi

pygobject_CODEGEN_DEFINES=""
AC_SUBST([pygobject_CODEGEN_DEFINES])

AS_AC_EXPAND(DATADIR, $datadir)

dnl libffi
AC_MSG_CHECKING(for ffi)
AC_ARG_WITH(ffi,
  AC_HELP_STRING([--without-ffi], [Disable libffi support]),
  with_libffi=$withval,
  with_libffi=auto)
if test x"$with_libffi" = xno ; then
  have_libffi=false
else
  PKG_CHECK_MODULES(FFI, libffi >= 3.0, have_libffi=yes, have_libffi=no)
fi
if test x"$with_libffi" = xyes && test x"$have_libffi" = xno ; then
  AC_MSG_ERROR([ffi requested, but not found])
fi
if test x"$have_libffi" = xyes; then
  AC_DEFINE(HAVE_FFI_H,1,[Have libffi include files])
fi
AC_MSG_RESULT([$have_libffi])
AM_CONDITIONAL(HAVE_LIBFFI, test "$have_libffi" = "yes")
AC_SUBST(FFI_CFLAGS)
AC_SUBST(FFI_LIBS)

dnl gio
PKG_CHECK_MODULES(GIO, gio-2.0 >= gio_required_version,
	have_gio=true, have_gio=false)
AC_SUBST(GIO_CFLAGS)
AC_SUBST(GIO_LIBS)
AM_CONDITIONAL(BUILD_GIO, $have_gio)
if test -n "$export_dynamic"; then
  GIO_LIBS=`echo $GIO_LIBS | sed -e "s/$export_dynamic//"`
fi

dnl giounix
PKG_CHECK_MODULES(GIOUNIX, gio-unix-2.0 >= giounix_required_version,
	have_giounix=true, have_giounix=false)
AC_SUBST(GIOUNIX_CFLAGS)
AC_SUBST(GIOUNIX_LIBS)
AM_CONDITIONAL(BUILD_GIOUNIX, $have_giounix)
if test -n "$export_dynamic"; then
  GIOUNIX_LIBS=`echo $GIOUNIX_LIBS | sed -e "s/$export_dynamic//"`
fi

dnl add required cflags ...
if test "x$GCC" = "xyes"; then
  JH_ADD_CFLAG([-Wall])
  JH_ADD_CFLAG([-fno-strict-aliasing])

  case $host_os in
  solaris*)
    ;;
  *)
    JH_ADD_CFLAG([-std=c9x])
    ;;
  esac

fi

AC_CONFIG_FILES(
  Makefile
  pygobject-2.0.pc
  pygobject-2.0-uninstalled.pc
  codegen/Makefile
  codegen/pygobject-codegen-2.0
  docs/Makefile
  docs/reference/entities.docbook
  docs/xsl/fixxref.py
  glib/Makefile
  gobject/Makefile
  gio/Makefile
  examples/Makefile
  tests/Makefile
  PKG-INFO)
AC_OUTPUT

echo
echo "libffi support: $have_libffi"
echo
