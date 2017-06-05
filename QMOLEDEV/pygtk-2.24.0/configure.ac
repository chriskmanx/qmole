-*- mode: m4 -*-
AC_PREREQ(2.52)

dnl the pygtk version number
m4_define(pygtk_major_version, 2)
m4_define(pygtk_minor_version, 24)
m4_define(pygtk_micro_version, 0)
m4_define(pygtk_version, pygtk_major_version.pygtk_minor_version.pygtk_micro_version)

dnl versions of packages we require ...
m4_define(glib_required_version,       2.8.0)
m4_define(pango_required_version,      1.16.0)
m4_define(atk_required_version,        1.12.0)
m4_define(gtk_required_version,        2.8.0)
m4_define(libglade_required_version,   2.5.0)
m4_define(gtkunixprint_required_version,  2.9.0)
m4_define(pycairo_required_version,    1.0.2) dnl or 1.1.7
m4_define(pygobject_required_version,  2.21.3)

AC_INIT(pygtk, pygtk_version,
        [http://bugzilla.gnome.org/enter_bug.cgi?product=pygtk])
AC_SUBST(ACLOCAL_AMFLAGS, "-I m4 -I .")

AC_DEFINE(PYGTK_MAJOR_VERSION, pygtk_major_version, [PyGtk major version])
AC_SUBST(PYGTK_MAJOR_VERSION, pygtk_major_version)
AC_DEFINE(PYGTK_MINOR_VERSION, pygtk_minor_version, [PyGtk minor version])
AC_SUBST(PYGTK_MINOR_VERSION, pygtk_minor_version)
AC_DEFINE(PYGTK_MICRO_VERSION, pygtk_micro_version, [PyGtk micro version])
AC_SUBST(PYGTK_MICRO_VERSION, pygtk_micro_version)

AC_CONFIG_SRCDIR([gtk/gtkmodule.c])
AM_CONFIG_HEADER(config.h)

AM_INIT_AUTOMAKE
AC_CHECK_FUNCS(bind_textdomain_codeset)

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

AM_PATH_PYTHON(2.3.5)

AM_CHECK_PYTHON_HEADERS(,[AC_MSG_ERROR(could not find Python headers)])

AC_MSG_CHECKING([for PySignal_SetWakeupFd in Python.h])
py_prefix=`$PYTHON -c "import sys; print sys.prefix"`
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

AC_ARG_ENABLE(thread,
  AC_HELP_STRING([--disable-thread], [Disable pygtk threading support]),,
  enable_thread=yes)

AM_CHECK_PYMOD(thread,,,enable_thread=no)

dnl Building documentation
AC_ARG_ENABLE(docs,
  AC_HELP_STRING([--enable-docs], [Enable documentation building]),enable_docs=$enableval,
  enable_docs=no)
if test "${enable_docs}" != no; then
  dnl Check for xsltproc
  AC_PATH_PROG([XSLTPROC], [xsltproc])
  if test -z "$XSLTPROC"; then
    enable_docs=no
  fi
fi

AM_CONDITIONAL(ENABLE_DOCS, test x$enable_docs != xno)

AC_MSG_CHECKING(whether to enable threading in pygtk)
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
AM_PATH_GLIB_2_0(glib_required_version,,[AC_MSG_ERROR(gobject is required to build pygtk?)],gobject $extra_mods)
if test -n "$export_dynamic"; then
  GLIB_LIBS=`echo $GLIB_LIBS | sed -e "s/$export_dynamic//"`
fi

PKG_CHECK_MODULES(PYGOBJECT, [pygobject-2.0 >= pygobject_required_version])

PYGOBJECT_INCLUDEDIR=`$PKG_CONFIG --variable=pygtkincludedir pygobject-2.0`
AC_SUBST(PYGOBJECT_INCLUDEDIR)

PYGOBJECT_DATADIR=`$PKG_CONFIG --variable=datadir pygobject-2.0`
AC_SUBST(PYGOBJECT_DATADIR)


dnl PyGObject defs
AC_SUBST(PYGOBJECT_DEFSDIR)
PYGOBJECT_DEFSDIR=`$PKG_CONFIG --variable defsdir pygobject-2.0`

AC_MSG_CHECKING([for gio-types.defs])
if test -n "$PYGOBJECT_DEFSDIR" && test -e "$PYGOBJECT_DEFSDIR/gio-types.defs"; then
  AM_CONDITIONAL(HAVE_GIO_TYPES_DEFS, true)
  AC_MSG_RESULT($PYGOBJECT_DEFSDIR/gio-types.defs)
else
  AM_CONDITIONAL(HAVE_GIO_TYPES_DEFS, false)
  AC_MSG_RESULT(no)
fi


dnl Documentation
PYGOBJECT_FIXXREF="$PYTHON `$PKG_CONFIG --variable=fixxref pygobject-2.0`"
AC_SUBST(PYGOBJECT_FIXXREF)

PYGOBJECT_PYGDOCS="`$PKG_CONFIG --variable=pygdocs pygobject-2.0`"
AC_SUBST(PYGOBJECT_PYGDOCS)

dnl atk
PKG_CHECK_MODULES(ATK, atk >= atk_required_version,
	have_atk=true, have_atk=false)
AC_SUBST(ATK_CFLAGS)
AC_SUBST(ATK_LIBS)
AM_CONDITIONAL(BUILD_ATK, $have_atk)
if test -n "$export_dynamic"; then
  ATK_LIBS=`echo $ATK_LIBS | sed -e "s/$export_dynamic//"`
fi

dnl pango
PKG_CHECK_MODULES(PANGO, pango >= pango_required_version,
	have_pango=true, have_pango=false)
AC_SUBST(PANGO_CFLAGS)
AC_SUBST(PANGO_LIBS)
AM_CONDITIONAL(BUILD_PANGO, $have_pango)
if test -n "$export_dynamic"; then
  PANGO_LIBS=`echo $PANGO_LIBS | sed -e "s/$export_dynamic//"`
fi

dnl codegen from PyGObject
AC_SUBST([CODEGENDIR])

AC_MSG_CHECKING([for codegen])
CODEGENDIR=`$PKG_CONFIG --variable codegendir pygobject-2.0`

if test -z $CODEGENDIR; then
  AC_MSG_ERROR([unable to find PyGObject codegen])
else
  AC_MSG_RESULT($CODEGENDIR)
fi

dnl pycairo
PYGTK_CODEGEN_DEFINES=""
AC_SUBST([PYGTK_CODEGEN_DEFINES])

gtk_version=""
PKG_CHECK_MODULES(PYCAIRO, [pycairo >= pycairo_required_version],
        have_pycairo=true, have_pycairo=false)
AC_SUBST(PYCAIRO_CFLAGS)
AC_SUBST(PYCAIRO_LIBS)
if test "x$have_pycairo" = "xtrue"; then
  PYGTK_CODEGEN_DEFINES="-DHAVE_PYCAIRO $PYGTK_CODEGEN_DEFINES";
  AC_DEFINE([HAVE_PYCAIRO], [1], [Define to 1 if pycairo is available])

  AC_DEFINE_UNQUOTED(PYCAIRO_MAJOR_VERSION, [`pkg-config --modversion pycairo | cut -d '.' -f 1`], dnl
                     [pycairo major version])
  AC_DEFINE_UNQUOTED(PYCAIRO_MINOR_VERSION, [`pkg-config --modversion pycairo | cut -d '.' -f 2`], dnl
                     [pycairo minor version])
  AC_DEFINE_UNQUOTED(PYCAIRO_MICRO_VERSION, [`pkg-config --modversion pycairo | cut -d '.' -f 3`], dnl
                     [pycairo micro version])

  dnl pangocairo
  PKG_CHECK_MODULES(PANGOCAIRO,
          [pangocairo >= pango_required_version,
           pycairo >= pycairo_required_version],
	  have_pangocairo=true, have_pangocairo=false)
  if test ! $have_pangocairo
  then
    AC_MSG_RESULT(no)
  fi
  AC_SUBST(PANGOCAIRO_CFLAGS)
  AC_SUBST(PANGOCAIRO_LIBS)
  AM_CONDITIONAL(BUILD_PANGOCAIRO, $have_pangocairo)

  if test -n "$export_dynamic"; then
    PANGOCAIRO_LIBS=`echo $PANGOCAIRO_LIBS | sed -e "s/$export_dynamic//"`
  fi

  dnl gtk+
  PKG_CHECK_MODULES(GTK, gtk+-2.0 >= gtk_required_version,
          HAVE_GTK="yes", HAVE_GTK="no")
  if test "x$HAVE_GTK" = "xyes"; then
     have_gtk=true
  else
     have_gtk=false
     AC_MSG_RESULT(no)
  fi
  AM_CONDITIONAL(BUILD_GTK, $have_gtk)
  AC_SUBST(GTK_CFLAGS)
  AC_SUBST(GTK_LIBS)
  if test -n "$export_dynamic"; then
    GTK_LIBS=`echo $GTK_LIBS | sed -e "s/$export_dynamic//"`
  fi

  PKG_CHECK_MODULES(GTK210, gtk+-2.0 >= 2.10.0,
	  HAVE_GTK_210="yes", HAVE_GTK_210="no")
  if test "x$HAVE_GTK_210" = "xyes"; then
     gtk_version="2.10"
     have_gtk_210=true
  else
     AC_MSG_RESULT(no)
     gtk_version="2.8"
     have_gtk_210=false
  fi

  AM_CONDITIONAL(HAVE_GTK_2_10, $have_gtk_210)
  AC_SUBST(HAVE_GTK_2_10)
  AC_DEFINE([HAVE_GTK_2_10], [$have_gtk_210], [If Gtk 2.10 was found])

  dnl check for gtk 2.12
  PKG_CHECK_MODULES(GTK212, gtk+-2.0 >= 2.11.6,
	  HAVE_GTK_212="yes", HAVE_GTK_212="no")

  if test "x$HAVE_GTK_212" = "xyes"; then
     gtk_version="2.12"
     have_gtk_212=true
     PYGTK_CODEGEN_DEFINES="$PYGTK_CODEGEN_DEFINES -DHAVE_GTK_2_12"
  else
     AC_MSG_RESULT(no)
     have_gtk_212=false
  fi

  AM_CONDITIONAL(HAVE_GTK_2_12, $have_gtk_212)
  AC_SUBST(HAVE_GTK_2_12)
  AC_DEFINE([HAVE_GTK_2_12], [$have_gtk_212], [If Gtk 2.12 was found])

  dnl check for gtk 2.14
  PKG_CHECK_MODULES(GTK214, gtk+-2.0 >= 2.13.8,
	  HAVE_GTK_214="yes", HAVE_GTK_214="no")

  if test "x$HAVE_GTK_214" = "xyes"; then
     gtk_version="2.14"
     have_gtk_214=true
     PYGTK_CODEGEN_DEFINES="$PYGTK_CODEGEN_DEFINES -DHAVE_GTK_2_14"
  else
     AC_MSG_RESULT(no)
     have_gtk_214=false
  fi

  AM_CONDITIONAL(HAVE_GTK_2_14, $have_gtk_214)
  AC_SUBST(HAVE_GTK_2_14)
  AC_DEFINE([HAVE_GTK_2_14], [$have_gtk_214], [If Gtk 2.14 was found])

  dnl check for gtk 2.16
  PKG_CHECK_MODULES(GTK216, gtk+-2.0 >= 2.16.0,
	  HAVE_GTK_216="yes", HAVE_GTK_216="no")

  if test "x$HAVE_GTK_216" = "xyes"; then
     gtk_version="2.16"
     have_gtk_216=true
     PYGTK_CODEGEN_DEFINES="$PYGTK_CODEGEN_DEFINES -DHAVE_GTK_2_16"
  else
     AC_MSG_RESULT(no)
     have_gtk_216=false
  fi

  AM_CONDITIONAL(HAVE_GTK_2_16, $have_gtk_216)
  AC_SUBST(HAVE_GTK_2_16)
  AC_DEFINE([HAVE_GTK_2_16], [$have_gtk_216], [If Gtk 2.16 was found])
  
  dnl check for gtk 2.18
  PKG_CHECK_MODULES(GTK218, gtk+-2.0 >= 2.18.0,
	  HAVE_GTK_218="yes", HAVE_GTK_218="no")

  if test "x$HAVE_GTK_218" = "xyes"; then
     gtk_version="2.18"
     have_gtk_218=true
     PYGTK_CODEGEN_DEFINES="$PYGTK_CODEGEN_DEFINES -DHAVE_GTK_2_18"
  else
     AC_MSG_RESULT(no)
     have_gtk_218=false
  fi

  AM_CONDITIONAL(HAVE_GTK_2_18, $have_gtk_218)
  AC_SUBST(HAVE_GTK_2_18)
  AC_DEFINE([HAVE_GTK_2_18], [$have_gtk_218], [If Gtk 2.18 was found])

  dnl check for gtk 2.20
  PKG_CHECK_MODULES(GTK220, gtk+-2.0 >= 2.20.0,
	  HAVE_GTK_220="yes", HAVE_GTK_220="no")

  if test "x$HAVE_GTK_220" = "xyes"; then
     gtk_version="2.20"
     have_gtk_220=true
     PYGTK_CODEGEN_DEFINES="$PYGTK_CODEGEN_DEFINES -DHAVE_GTK_2_20"
  else
     AC_MSG_RESULT(no)
     have_gtk_220=false
  fi

  AM_CONDITIONAL(HAVE_GTK_2_20, $have_gtk_220)
  AC_SUBST(HAVE_GTK_2_20)
  AC_DEFINE([HAVE_GTK_2_20], [$have_gtk_220], [If Gtk 2.20 was found])

  dnl check for gtk 2.22
  PKG_CHECK_MODULES(GTK222, gtk+-2.0 >= 2.22.0,
	  HAVE_GTK_222="yes", HAVE_GTK_222="no")

  if test "x$HAVE_GTK_222" = "xyes"; then
     gtk_version="2.22"
     have_gtk_222=true
     PYGTK_CODEGEN_DEFINES="$PYGTK_CODEGEN_DEFINES -DHAVE_GTK_2_22"
  else
     AC_MSG_RESULT(no)
     have_gtk_222=false
  fi

  AM_CONDITIONAL(HAVE_GTK_2_22, $have_gtk_222)
  AC_SUBST(HAVE_GTK_2_22)
  AC_DEFINE([HAVE_GTK_2_22], [$have_gtk_222], [If Gtk 2.22 was found])

  dnl check for gtk 2.24
  PKG_CHECK_MODULES(GTK224, gtk+-2.0 >= 2.24.0,
	  HAVE_GTK_224="yes", HAVE_GTK_224="no")

  if test "x$HAVE_GTK_224" = "xyes"; then
     gtk_version="2.24"
     have_gtk_224=true
     PYGTK_CODEGEN_DEFINES="$PYGTK_CODEGEN_DEFINES -DHAVE_GTK_2_24"
  else
     AC_MSG_RESULT(no)
     have_gtk_224=false
  fi

  AM_CONDITIONAL(HAVE_GTK_2_24, $have_gtk_224)
  AC_SUBST(HAVE_GTK_2_24)
  AC_DEFINE([HAVE_GTK_2_24], [$have_gtk_224], [If Gtk 2.24 was found])

  dnl libglade
  have_libglade=false
  AC_ARG_WITH([glade], AS_HELP_STRING([--without-glade], [Build without glade library (default: test)]))
  if test "x$with_glade" != "xno"; then
    PKG_CHECK_MODULES(LIBGLADE, [libglade-2.0 >= libglade_required_version],
	  have_libglade=true, have_libglade=false)
  fi
  AC_SUBST(LIBGLADE_CFLAGS)
  AC_SUBST(LIBGLADE_LIBS)
  AM_CONDITIONAL(BUILD_LIBGLADE, $have_libglade)
  if test -n "$export_dynamic"; then
    LIBGLADE_LIBS=`echo $LIBGLADE_LIBS | sed -e "s/$export_dynamic//"`
  fi

  dnl gtkunixprint
  PKG_CHECK_MODULES(GTKUNIXPRINT, [gtk+-unix-print-2.0 >= gtkunixprint_required_version],
	  have_gtkunixprint=true, have_gtkunixprint=false)
  AC_SUBST(GTKUNIXPRINT_CFLAGS)
  AC_SUBST(GTKUNIXPRINT_LIBS)
  AM_CONDITIONAL(BUILD_GTKUNIXPRINT, $have_gtkunixprint)
  if test -n "$export_dynamic"; then
    GTKUNIXPRINT_LIBS=`echo $GTKUNIXPRINT_LIBS | sed -e "s/$export_dynamic//"`
  fi

else
  AC_MSG_RESULT(no)
  AC_MSG_RESULT(not checking for gtk due to missing pycairo)
  AM_CONDITIONAL(BUILD_PANGOCAIRO, false)
  AM_CONDITIONAL(BUILD_GTK, false)
  AM_CONDITIONAL(HAVE_GTK_2_10, false)
  AM_CONDITIONAL(HAVE_GTK_2_12, false)
  AM_CONDITIONAL(HAVE_GTK_2_14, false)
  AM_CONDITIONAL(HAVE_GTK_2_16, false)
  AM_CONDITIONAL(HAVE_GTK_2_18, false)
  AM_CONDITIONAL(HAVE_GTK_2_20, false)
  AM_CONDITIONAL(HAVE_GTK_2_22, false)
  AM_CONDITIONAL(HAVE_GTK_2_24, false)
  AM_CONDITIONAL(BUILD_LIBGLADE, false)
  AM_CONDITIONAL(BUILD_GTKUNIXPRINT, false)
  have_pangocairo=false
  have_gtk=false
  have_libglade=false
  have_gtkunixprint=false
fi

dnl Check for GDK target
AC_MSG_CHECKING([for GDK target])
gdk_target=`$PKG_CONFIG --variable target gdk-2.0`
if test -z $gdk_target; then
  AC_MSG_RESULT((unknown))
  AC_MSG_WARN([unable to determine the GDK target system; some functions may not be available])
else
  AC_MSG_RESULT($gdk_target)
fi

case $gdk_target in
        x11) PYGTK_CODEGEN_DEFINES="$PYGTK_CODEGEN_DEFINES -DGDK_TARGET_X11"
        ;;
esac


dnl checks to see if numpy is installed.
AC_ARG_ENABLE(numpy,
  AC_HELP_STRING([--disable-numpy], [Disable numeric python features]),,
  enable_numpy=yes)

if test "x$enable_numpy" != xno; then
  save_CPPFLAGS="$CPPFLAGS"
  numpy_INCLUDES=`$PYTHON -c "import numpy; print numpy.get_include()" 2> /dev/null`
  if test "x$numpy_INCLUDES" = "x"; then
    AC_MSG_WARN([Could not find a valid numpy installation, disabling.])
    enable_numpy=no
  else
    CPPFLAGS="$CPPFLAGS $PYTHON_INCLUDES -I$numpy_INCLUDES"
    AC_CHECK_HEADER([numpy/arrayobject.h],
      [AC_DEFINE(HAVE_NUMPY,,[whether to include numeric python support])],
      [enable_numpy=no],
      [#include <Python.h>])
    if test "x$enable_numpy" != "xno"; then
      CPPFLAGS="$save_CPPFLAGS -I$numpy_INCLUDES"
    fi
  fi
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
  pygtk-2.0.pc
  pygtk-codegen-2.0
  gtk/Makefile
  docs/Makefile
  docs/reference/entities.docbook
  examples/Makefile
  tests/Makefile
  pygtk.spec
  PKG-INFO)
AC_OUTPUT

echo
echo "The following modules will be built:"
echo
$have_atk && echo     atk
$have_pango && echo     pango
$have_pangocairo && echo     pangocairo
$have_gtk && echo     gtk with $gtk_version API
$have_libglade && echo     gtk.glade
$have_gtkunixprint && echo     gtk.unixprint

if test ! $have_atk || ! $have_pango || \
        ! $have_gtk || ! $have_libglade || \
        ! $have_pangocairo || ! $have_gtkunixprint; then
  echo
  echo "The following modules will NOT be built:"
  echo
  $have_atk || echo     atk
  $have_pango || echo     pango
  $have_pangocairo || echo     pangocairo
  $have_gtk || echo     gtk
  $have_libglade || echo     gtk.glade
  $have_gtkunixprint || echo     gtk.unixprint
fi

echo
echo "Numpy support: $enable_numpy"
echo
