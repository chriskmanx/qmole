dnl Process this file with autoconf to produce a configure script.

AC_PREREQ(2.63.2)

# Copied from glib
#
# Making releases:
#   glib_micro_version += 1;
#   glib_interface_age += 1;
#   glib_binary_age += 1;
# if any functions have been added, set glib_interface_age to 0.
# if backwards compatibility has been broken,
# set glib_binary_age _and_ glib_interface_age to 0.
#
# in easier to understand terms:
#
# <mclasen> on the stable branch, interface age == micro
# <mclasen> on the unstable (ie master), interface age = 0
m4_define(peas_major_version,  1)
m4_define(peas_minor_version,  2)
m4_define(peas_micro_version,  0)
m4_define(peas_interface_age,  0)

m4_define(peas_binary_age, [m4_eval(100 * peas_minor_version + peas_micro_version)])
m4_define(peas_version, [peas_major_version.peas_minor_version.peas_micro_version])

dnl libtool version related macros
m4_define(peas_lt_current, [m4_eval(100 * peas_minor_version + peas_micro_version - peas_interface_age)])
m4_define(peas_lt_revision, [peas_interface_age])
m4_define(peas_lt_age, [m4_eval(peas_binary_age - peas_interface_age)])

AC_INIT([libpeas],[peas_version],[http://bugzilla.gnome.org/enter_bug.cgi?product=libpeas],[libpeas],[http://live.gnome.org/Libpeas])

AC_CONFIG_HEADERS([config.h])
AC_CONFIG_SRCDIR([libpeas/peas-engine.c])
AC_CONFIG_MACRO_DIR([m4])

AC_DEFINE(PEAS_MAJOR_VERSION, peas_major_version, [Gedit Plugins Engine major version])
AC_SUBST(PEAS_MAJOR_VERSION, peas_major_version)
AC_DEFINE(PEAS_MINOR_VERSION, peas_minor_version, [Gedit Plugins Engine minor version])
AC_SUBST(PEAS_MINOR_VERSION, peas_minor_version)
AC_DEFINE(PEAS_MICRO_VERSION, peas_micro_version, [Gedit Plugins Engine micro version])
AC_SUBST(PEAS_MICRO_VERSION, peas_micro_version)

AC_SUBST(PEAS_VERSION, peas_version)

AM_INIT_AUTOMAKE([1.11 tar-ustar dist-xz no-dist-gzip -Wno-portability])
AM_MAINTAINER_MODE([enable])

AM_SILENT_RULES([yes])

dnl ================================================================
dnl Basic checks
dnl ================================================================

# Checks for programs.
AC_PROG_CC
AC_PROG_INSTALL
AC_PROG_LN_S

# Initialize libtool
LT_PREREQ([2.2.6])
LT_INIT([disable-static])


AC_CHECK_FUNCS(fsync)

dnl ================================================================
dnl Gettext stuff.
dnl ================================================================

IT_PROG_INTLTOOL([0.40.0])

AM_GNU_GETTEXT_VERSION([0.17])
AM_GNU_GETTEXT([external])

GETTEXT_PACKAGE=libpeas
AC_SUBST(GETTEXT_PACKAGE)
AC_DEFINE_UNQUOTED(GETTEXT_PACKAGE,"$GETTEXT_PACKAGE", [Gettext package])

dnl ================================================================
dnl Minimum libpeas requirements (GObject and Introspection)
dnl ================================================================

PKG_PROG_PKG_CONFIG

GLIB_REQUIRED=2.18.0
GOBJECT_REQUIRED=2.23.6
INTROSPECTION_REQUIRED=0.10.1

PKG_CHECK_MODULES(PEAS, [
	glib-2.0 >= $GLIB_REQUIRED
	gobject-2.0 >= $GOBJECT_REQUIRED
	gmodule-2.0 >= $GLIB_REQUIRED
	gobject-introspection-1.0 >= $INTROSPECTION_REQUIRED
])

GOBJECT_INTROSPECTION_REQUIRE($INTROSPECTION_REQUIRED)

AC_SUBST(GLIB_REQUIRED)
AC_SUBST(GOBJECT_REQUIRED)
AC_SUBST(INTROSPECTION_REQUIRED)

dnl ================================================================
dnl Build libpeas-gtk
dnl ================================================================

GTK_REQUIRED=2.91.1

AC_ARG_ENABLE(gtk,
	      AS_HELP_STRING([--enable-gtk],[Build libpeas-gtk for Gtk widgetry]),
	      [enable_gtk=$enableval],
	      [enable_gtk=yes])

if test "$enable_gtk" = "yes"; then
	PKG_CHECK_MODULES(PEAS_GTK, [
		gtk+-3.0 >= $GTK_REQUIRED
	])
fi

AC_SUBST(GTK_REQUIRED)

AM_CONDITIONAL([ENABLE_GTK],[test "x$enable_gtk" = "xyes"])

dnl ================================================================
dnl Check for native OSX
dnl ================================================================
AC_MSG_CHECKING([for native Mac OS X])


gdk_windowing=`$PKG_CONFIG --variable=target gdk-3.0`

if test "$gdk_windowing" = "quartz"; then
	os_osx=yes
else
	os_osx=no
fi
AC_MSG_RESULT([$os_osx])
AM_CONDITIONAL(OS_OSX, test "$os_osx" = "yes")

if test "$os_osx" = "yes"; then
	AC_DEFINE([OS_OSX],[1],[Defined if os is Mac OSX])

	PKG_CHECK_MODULES(IGE_MAC, ige-mac-integration)
fi

dnl ================================================================
dnl Check for GDB
dnl ================================================================

if test "x$GCC" = "xno"; then
  enable_gdb=no
else
  AC_PATH_PROG([enable_gdb], [gdb], [no])
fi

AM_CONDITIONAL([GDB_ENABLED], [test "x$enable_gdb" != "xno"])

dnl ================================================================
dnl Check for Valgrind
dnl ================================================================

AC_PATH_PROG([enable_valgrind], [valgrind], [no])

AM_CONDITIONAL([VALGRIND_ENABLED], [test "x$enable_valgrind" != "xno"])

dnl ================================================================
dnl Coverage testing
dnl ================================================================

AC_ARG_ENABLE([gcov],
	      [AS_HELP_STRING([--enable-gcov],
			      [Whether to enable coverage testing (requires gcc and gcov) [default=no]])],
	      [enable_gcov=$enable_val],
	      [enable_gcov=no])

if test "x$enable_gcov" = "xno"; then
        enable_gcov="no (disabled, use --enable-gcov to enable)"
        AC_MSG_CHECKING([whether to build with coverage testing])
else
        AC_PATH_PROG([enable_gcov], [gcov], [no])
        AC_MSG_CHECKING([whether to build with coverage testing])

        if test "x$enable_gcov" = "xno"; then
                AC_MSG_ERROR([You need gcov to build with coverage testing])
        elif test "x$GCC" = "xno"; then
                AC_MSG_ERROR([You need to use gcc to build with coverage testing])
        else
                enable_gcov=yes
                GCOV_CFLAGS="-g -O0 -fprofile-arcs -ftest-coverage"
                GCOV_LDFLAGS="-lgcov"
        fi
fi
AC_MSG_RESULT([$enable_gcov])

AM_CONDITIONAL([GCOV_ENABLED], [test "x$enable_gcov" = "xyes"])
AC_SUBST([GCOV_CFLAGS])
AC_SUBST([GCOV_LDFLAGS])

dnl ================================================================
dnl Glade
dnl ================================================================

AC_ARG_ENABLE([glade-catalog],
	[AS_HELP_STRING([--enable-glade-catalog],
		[Install a glade catalog file [default=auto]])],
	[enable_glade_catalog=$enableval],
	[enable_glade_catalog=auto])

if test "x$enable_glade_catalog" = "xno"; then
  found_glade_catalog="no (disabled, use --enable-glade-catalog to enable)"
else
  AC_MSG_CHECKING([for Glade.])

  PKG_CHECK_EXISTS([gladeui-2.0],
			 [found_glade_catalog=yes],
			 [found_glade_catalog=no])
fi

if test "x$enable_glade_catalog" = "xyes" -a "x$found_glade_catalog" = "xno"; then
  AC_MSG_ERROR([You need to have gladeui-2.0 installed to build libpeas])
fi
AC_MSG_RESULT([$found_glade_catalog])

if test "x$found_glade_catalog" = "xyes"; then
  AC_MSG_CHECKING([for Glade catalog dir])
  GLADE_DATA_DIR=`$PKG_CONFIG --variable=datadir gladeui-2.0 2>&AS_MESSAGE_LOG_FD`
  GLADE_CATALOG_DIR=`$PKG_CONFIG --variable=catalogdir gladeui-2.0 2>&AS_MESSAGE_LOG_FD | sed -e "s#^$GLADE_DATA_DIR#$datadir#"`
  AC_MSG_RESULT([$GLADE_CATALOG_DIR])
  AC_SUBST(GLADE_CATALOG_DIR)
fi

AM_CONDITIONAL([ENABLE_GLADE_CATALOG],[test "x$found_glade_catalog" = "xyes"])

dnl ================================================================
dnl Seed Javascript Engine
dnl ================================================================

SEED_REQUIRED=2.91.91

AC_ARG_ENABLE(seed,
	      AS_HELP_STRING([--enable-seed],[Enable Seed support]),
	      [enable_seed=$enableval],
	      [enable_seed=auto])

AC_MSG_CHECKING([for Seed JS availability.])

if test "x$enable_seed" = "xno"; then
	found_seed="no (disabled, use --enable-seed to enable)"
else
	PKG_CHECK_EXISTS([seed >= $SEED_REQUIRED],
			 [found_seed=yes],
			 [found_seed=no])
fi

if test "$enable_seed" = "yes" -a "$found_seed" = "no"; then
	AC_MSG_ERROR([You need to have seed >= $SEED_REQUIRED installed to build libpeas])
fi
AC_MSG_RESULT([$found_seed])

if test "$found_seed" = "yes"; then
	SEED_CFLAGS=`$PKG_CONFIG --cflags seed`
	SEED_LIBS=`$PKG_CONFIG --libs seed`
	AC_SUBST(SEED_CFLAGS)
	AC_SUBST(SEED_LIBS)

	AC_DEFINE(ENABLE_SEED,1,[Define to compile with Seed support])
fi

AM_CONDITIONAL([ENABLE_SEED],[test "x$found_seed" = "xyes"])

dnl ================================================================
dnl GJS Javascript Engine
dnl ================================================================

GJS_REQUIRED=1.29.16

AC_ARG_ENABLE(gjs,
	      AS_HELP_STRING([--enable-gjs],[Enable GJS support]),
	      [enable_gjs=$enableval],
	      [enable_gjs=auto])

AC_MSG_CHECKING([for GJS JS availability.])

if test "x$enable_gjs" = "xno"; then
	found_gjs="no (disabled, use --enable-gjs to enable)"
else
	PKG_CHECK_EXISTS([gjs-internals-1.0 >= $GJS_REQUIRED],
			 [found_gjs=yes],
			 [found_gjs=no])
fi

if test "$enable_gjs" = "yes" -a "$found_gjs" = "no"; then
	AC_MSG_ERROR([You need to have gjs-internals-1.0 >= $GJS_REQUIRED installed to build libpeas])
fi
AC_MSG_RESULT([$found_gjs])

if test "$found_gjs" = "yes"; then
	GJS_CFLAGS=`$PKG_CONFIG --cflags gjs-internals-1.0`
	GJS_LIBS=`$PKG_CONFIG --libs gjs-internals-1.0`
	AC_SUBST(GJS_CFLAGS)
	AC_SUBST(GJS_LIBS)

	AC_DEFINE(ENABLE_GJS,1,[Define to compile with GJS support])
fi

AM_CONDITIONAL([ENABLE_GJS],[test "x$found_gjs" = "xyes"])

dnl ================================================================
dnl Python
dnl ================================================================

PYTHON_REQUIRED=2.5.2

dnl Used by the Python loader to check the PyGObject version at runtime
PYGOBJECT_MAJOR_VERSION=3
PYGOBJECT_MINOR_VERSION=0
PYGOBJECT_MICRO_VERSION=0
PYGOBJECT_REQUIRED=$PYGOBJECT_MAJOR_VERSION.$PYGOBJECT_MINOR_VERSION.$PYGOBJECT_MICRO_VERSION

AC_ARG_ENABLE(python,
	      AS_HELP_STRING([--enable-python],[Enable Python support]),
	      [enable_python=$enableval],
	      [enable_python=auto])

if test "$enable_python" = "no"; then
	found_python="no (disabled, use --enable-python to enable)"
else
	AM_PATH_PYTHON($PYTHON_REQUIRED,
		       [found_python=yes],
		       [found_python="no (python interpretor >= ${PYTHON_REQUIRED} not found"])

	if test "$found_python" = "yes"; then
		AC_PATH_TOOL(PYTHON_CONFIG, "python${PYTHON_VERSION}-config")
		if test -z "$PYTHON_CONFIG"; then
			AC_PATH_TOOL(PYTHON_CONFIG, "python-config-${PYTHON_VERSION}")
			if test -z "$PYTHON_CONFIG"; then
				found_python="no (python${PYTHON_VERSION}-config not found)"
			fi
		fi
	fi

	if test "$found_python" = "yes"; then
		save_CPPFLAGS="${CPPFLAGS}"
		CPPFLAGS="$CPPFLAGS `$PYTHON_CONFIG --includes`"
		AC_CHECK_HEADER(Python.h,,[found_python="no (Python headers not found)"])
		CPPFLAGS="${save_CPPFLAGS}"
	fi

	AC_MSG_CHECKING([for PyGObject availability.])
	if test "$found_python" = "yes"; then
		PKG_CHECK_EXISTS([pygobject-3.0 >= $PYGOBJECT_REQUIRED],,
				 [found_python="no (pygobject >= $PYGOBJECT_REQUIRED not found)"])
	fi
	AC_MSG_RESULT([$found_python])
fi

if test "$enable_python" = "yes" -a "$found_python" != "yes"; then
	AC_MSG_ERROR([$found_python])
fi

if test "$found_python" = "yes"; then
	PYTHON_CFLAGS="`$PKG_CONFIG --cflags pygobject-3.0` `$PYTHON_CONFIG --includes`"
	PYTHON_LIBS="`$PKG_CONFIG --libs pygobject-3.0` `$PYTHON_CONFIG --libs`"
	AC_SUBST(PYTHON_CFLAGS)
	AC_SUBST(PYTHON_LIBS)

	AC_DEFINE_UNQUOTED(PYGOBJECT_MAJOR_VERSION,[$PYGOBJECT_MAJOR_VERSION], [PyGObject major version.])
	AC_DEFINE_UNQUOTED(PYGOBJECT_MINOR_VERSION,[$PYGOBJECT_MINOR_VERSION], [PyGObject minor version.])
	AC_DEFINE_UNQUOTED(PYGOBJECT_MICRO_VERSION,[$PYGOBJECT_MICRO_VERSION], [PyGObject micro version.])
	AC_DEFINE(ENABLE_PYTHON,1,[Define to compile with Python support])
fi

AM_CONDITIONAL([ENABLE_PYTHON],[test "$found_python" = "yes"])

dnl ================================================================
dnl Vala
dnl ================================================================

# AM_PROG_VALAC errors if a different version of vala is found
# so we copied, renamed and fixed it
# Autoconf support for the Vala compiler
# Copyright (C) 2008, 2009 Free Software Foundation, Inc.
#
# This file is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.
AC_DEFUN([CHECK_VALAC],
[AC_PATH_PROG([VALAC], [valac], [])
 AS_IF([test -z "$VALAC"],
   [AC_MSG_WARN([No Vala compiler found.  You will not be able to compile .vala source files.])],
   [AS_IF([test -n "$1"],
      [AC_MSG_CHECKING([$VALAC is at least version $1])
       am__vala_version=`$VALAC --version | sed 's/Vala  *//'`
       AS_VERSION_COMPARE([$1], ["$am__vala_version"],
         [AC_MSG_RESULT([yes])],
         [AC_MSG_RESULT([yes])],
         [AC_MSG_RESULT([no])])])])
])

VALA_REQUIRED=0.11.1

AC_ARG_ENABLE([vala],
	      AS_HELP_STRING([--enable-vala],[Enable Vala support]),
	      [enable_vala=$enableval],
	      [enable_vala=auto])

if test "$enable_vala" = "no"; then
	found_vala="no (disabled, use --enable-vala to enable)"
else
	CHECK_VALAC($VALA_REQUIRED)
	if test "$VALAC" = ""; then
		found_vala="no (valac >= $VALA_REQUIRED not found)"
	else
		found_vala="yes"
	fi
fi

if test "$enable_vala" = "yes" -a "$found_vala" != "yes"; then
	AC_MSG_ERROR([$found_vala])
fi

AM_CONDITIONAL([ENABLE_VALA],[test "$found_vala" = "yes"])

dnl ================================================================
dnl Documentation
dnl ================================================================

GTK_DOC_CHECK([1.11],[--flavour no-tmpl])

dnl ================================================================
dnl Libtool
dnl ================================================================

LT_CURRENT=peas_lt_current
LT_REVISION=peas_lt_revision
LT_AGE=peas_lt_age
AC_SUBST(LT_CURRENT)
AC_SUBST(LT_REVISION)
AC_SUBST(LT_AGE)

# The reasons for -rpath /nowhere can be found at:
# http://www.mail-archive.com/automake@gnu.org/msg10725.htm

PLUGIN_LIBTOOL_FLAGS="-module -avoid-version"
TEST_PLUGIN_LIBTOOL_FLAGS="$PLUGIN_LIBTOOL_FLAGS -rpath /nowhere"
LOADER_LIBTOOL_FLAGS="-module -avoid-version -export-symbols-regex peas_register_types"
AC_SUBST(PLUGIN_LIBTOOL_FLAGS)
AC_SUBST(TEST_PLUGIN_LIBTOOL_FLAGS)
AC_SUBST(LOADER_LIBTOOL_FLAGS)

dnl ================================================================
dnl Misc
dnl ================================================================

AC_DEFINE(G_LOG_DOMAIN, "libpeas", [Log domain])

AC_PATH_PROG(GLIB_GENMARSHAL, glib-genmarshal)
AC_PATH_PROG(GLIB_MKENUMS, glib-mkenums)

GNOME_COMPILE_WARNINGS([maximum])
# Use --enable-maintainer-mode to disabled deprecated symbols
GNOME_MAINTAINER_MODE_DEFINES

AC_ARG_ENABLE([deprecation],
             AS_HELP_STRING([--disable-deprecation],[Disable deprecated libpeas features]),
             [enable_deprecation=$enableval],
             [enable_deprecation=$USE_MAINTAINER_MODE])
if test "$enable_deprecation" = "yes"; then
       AC_MSG_NOTICE([disabling deprecated libpeas features])
       AC_DEFINE([PEAS_DISABLE_DEPRECATED_FEATURES],[1],[Disable deprecated features])
fi


AC_CONFIG_FILES([
Makefile
docs/Makefile
docs/reference/Makefile
docs/reference/version.xml
libpeas/Makefile
libpeas-gtk/Makefile
loaders/Makefile
loaders/c/Makefile
loaders/gjs/Makefile
loaders/python/Makefile
loaders/seed/Makefile
data/Makefile
data/glade/Makefile
data/icons/Makefile
data/libpeas-1.0.pc
data/libpeas-gtk-1.0.pc
peas-demo/Makefile
peas-demo/plugins/Makefile
peas-demo/plugins/gjshello/Makefile
peas-demo/plugins/helloworld/Makefile
peas-demo/plugins/pythonhello/Makefile
peas-demo/plugins/secondtime/Makefile
peas-demo/plugins/seedhello/Makefile
peas-demo/plugins/valahello/Makefile
po/Makefile.in
tests/Makefile
tests/libpeas/Makefile
tests/libpeas/plugins/Makefile
tests/libpeas/plugins/extension-c/Makefile
tests/libpeas/plugins/extension-js/Makefile
tests/libpeas/plugins/extension-python/Makefile
tests/libpeas/introspection/Makefile
tests/libpeas/testing/Makefile
tests/libpeas-gtk/Makefile
tests/libpeas-gtk/plugins/Makefile
tests/libpeas-gtk/plugins/builtin-configurable/Makefile
tests/libpeas-gtk/plugins/configurable/Makefile
tests/libpeas-gtk/plugins/valid-custom-icon/Makefile
tests/libpeas-gtk/testing/Makefile
tests/plugins/Makefile
tests/plugins/builtin/Makefile
tests/plugins/has-dep/Makefile
tests/plugins/loadable/Makefile
tests/plugins/self-dep/Makefile
tests/testing-util/Makefile
])

AC_OUTPUT

echo "

Configuration:

        Source code location          : ${srcdir}
        Compiler                      : ${CC}
        Installation prefix           : ${prefix}
        Build libpeas-gtk             : ${enable_gtk}
        Coverage testing              : ${enable_gcov}
        Glade Catalog                 : ${found_glade_catalog}
        Disable deprecated features   : ${enable_deprecation}

Languages support:

        Python support                : ${found_python}
        Javascript support (Seed)     : ${found_seed}
        Javascript support (GJS)      : ${found_gjs}
        Vala support                  : ${found_vala}
"
