dnl Process this file with autoconf to produce a configure script.

AC_PREREQ(2.64)

m4_define(gtksourceview_major_version, 3)
m4_define(gtksourceview_minor_version, 2)
m4_define(gtksourceview_micro_version, 3)
m4_define(gtksourceview_version, gtksourceview_major_version.gtksourceview_minor_version.gtksourceview_micro_version)

AC_INIT([gtksourceview],[gtksourceview_version],[http://bugzilla.gnome.org/enter_bug.cgi?product=gtksourceview],[gtksourceview],[http://projects.gnome.org/gtksourceview/])

AC_CONFIG_SRCDIR([gtksourceview/gtksourcebuffer.h])
AC_CONFIG_HEADER([config.h])
AC_CONFIG_MACRO_DIR([m4])

AM_INIT_AUTOMAKE([1.11 tar-ustar dist-xz no-dist-gzip -Wno-portability])
AM_MAINTAINER_MODE([enable])

m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])

# Check for programs
AC_PROG_CC
AC_PROG_INSTALL

#Initialize libtool
LT_PREREQ([2.2.6])
LT_INIT([disable-static])

# Check for header files
AC_CHECK_HEADERS([unistd.h])

# Dependencies
GTK_REQUIRED_VERSION=3.0.0
LIBXML_REQUIRED_VERSION=2.6.0
GLADE_UI_REQUIRED=3.9

AC_SUBST(GTK_REQUIRED)
AC_SUBST(GTK_REQUIRED_VERSION)
AC_SUBST(LIBXML_REQUIRED_VERSION)

# Pull glib-genmarshal & co.
AC_PATH_PROG(GLIB_GENMARSHAL, glib-genmarshal)
AC_PATH_PROG(GLIB_MKENUMS, glib-mkenums)

# Compile warnings
GNOME_COMPILE_WARNINGS([maximum])

if test "$USE_MAINTAINER_MODE" = "yes"; then
	enable_deprecations=yes
else
	enable_deprecations=no
fi

AC_ARG_ENABLE(deprecations,
              [AS_HELP_STRING([--enable-deprecations],
                              [warn about deprecated usages [default=no]])],
              [enable_deprecations=$enableval], [])

if test "$enable_deprecations" = "yes"; then
	DISABLE_DEPRECATED_CFLAGS="\
-DG_DISABLE_DEPRECATED \
-DGDK_DISABLE_DEPRECATED \
-DGTK_DISABLE_DEPRECATED \
-DGDK_PIXBUF_DISABLE_DEPRECATED \
-DGNOME_DISABLE_DEPRECATED \
-DGSEAL_ENABLE"

	AC_SUBST(DISABLE_DEPRECATED_CFLAGS)
fi

PKG_CHECK_MODULES(DEP, [
	gio-2.0 >= 2.28.0
	gtk+-3.0 >= $GTK_REQUIRED_VERSION
	libxml-2.0 >= $LIBXML_REQUIRED_VERSION
])

dnl Check to enable completion providers
AC_ARG_ENABLE(providers,
	    AS_HELP_STRING([--enable-completion-providers],
	    		   [Enable building completion providers (words)]),,
	    enable_providers=yes)

AM_CONDITIONAL([ENABLE_PROVIDERS], [ test "$enable_providers" = "yes" ])

dnl check for native osx
AC_MSG_CHECKING([for native Mac OS X])

gdk_windowing=`$PKG_CONFIG --variable=target gdk-3.0`

if test "$gdk_windowing" = "quartz"; then
	os_osx=yes
else
	os_osx=no
fi
AC_MSG_RESULT([$os_osx])
AM_CONDITIONAL([OS_OSX], [ test "$os_osx" = "yes" ])

if test "$os_osx" = "yes"; then
	AC_DEFINE([OS_OSX], [1], [Defined if os is Mac OSX])

	PKG_CHECK_MODULES(IGE_MAC, ige-mac-integration)
fi

# Check for Glade3
AC_ARG_ENABLE([glade-catalog],
	[AS_HELP_STRING([--enable-glade-catalog],
		[Install a glade catalog file [default=no]])],
	[glade_catalog=$enableval],
	[glade_catalog=no])

AM_CONDITIONAL([GLADE_CATALOG], [ test "$glade_catalog" = "yes" ])

AS_IF([ test "$glade_catalog" = "yes" ],
	[PKG_CHECK_EXISTS(GLADE, [gladeui-2.0 >= $GLADE_UI_REQUIRED])
	 AC_MSG_CHECKING([for Glade catalog dir])
	 GLADE_CATALOG_DIR=`$PKG_CONFIG --variable=catalogdir gladeui-2.0 2>&AS_MESSAGE_LOG_FD`
	 AC_MSG_RESULT([$GLADE_CATALOG_DIR])
	 AC_SUBST(GLADE_CATALOG_DIR)])

# i18N stuff
IT_PROG_INTLTOOL([0.40])

AM_GNU_GETTEXT_VERSION([0.17])
AM_GNU_GETTEXT([external])

AC_SUBST([GETTEXT_PACKAGE], [gtksourceview-3.0])
AC_DEFINE_UNQUOTED([GETTEXT_PACKAGE], ["$GETTEXT_PACKAGE"], [Define to the gettext package name.])

# Documentation
GTK_DOC_CHECK([1.11], [--flavour=no-tmpl])

# Introspection
GOBJECT_INTROSPECTION_CHECK([0.9.0])

# test-widget uses this to find lang files and gtksourcebuffer.c
ABS_TOP_SRCDIR=`cd $srcdir && pwd`
AC_SUBST(ABS_TOP_SRCDIR)

# Output files
AC_CONFIG_FILES([
gtksourceview-3.0.pc
gtksourceview-zip
Makefile
data/Makefile
data/glade/Makefile
data/language-specs/Makefile
data/styles/Makefile
docs/Makefile
docs/reference/Makefile
docs/reference/version.xml
gtksourceview/Makefile
gtksourceview/completion-providers/Makefile
gtksourceview/completion-providers/words/Makefile
po/Makefile.in
tests/Makefile
])

AC_OUTPUT


echo "

Configuration:

	Source code location:	${srcdir}
	Compiler:		${CC}
	Completion Providers:	${enable_providers}
	Glade Catalog:		${glade_catalog}
	GObject introspection:	${found_introspection}
"
