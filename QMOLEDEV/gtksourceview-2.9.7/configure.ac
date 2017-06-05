dnl Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)

m4_define(gtksourceview_major_version, 2)
m4_define(gtksourceview_minor_version, 9)
m4_define(gtksourceview_micro_version, 7)
m4_define(gtksourceview_version, gtksourceview_major_version.gtksourceview_minor_version.gtksourceview_micro_version)

AC_INIT(gtksourceview, gtksourceview_version, http://bugzilla.gnome.org/enter_bug.cgi?product=gtksourceview)

AC_CONFIG_SRCDIR([gtksourceview/gtksourcebuffer.h])
AM_CONFIG_HEADER(config.h)

AM_INIT_AUTOMAKE([1.8 dist-bzip2 no-dist-gzip])

dnl make sure we keep ACLOCAL_FLAGS around for maintainer builds to work
AC_SUBST(ACLOCAL_AMFLAGS, "$ACLOCAL_FLAGS")

AM_MAINTAINER_MODE
m4_ifdef([AM_SILENT_RULES],[AM_SILENT_RULES([yes])])

AC_ISC_POSIX
AC_PROG_CC
AC_PROG_INSTALL
AC_DISABLE_STATIC
AM_PROG_LIBTOOL
IT_PROG_INTLTOOL([0.35.0])

GTK_DOC_CHECK([1.0],[--flavour=no-tmpl])

# Check for headers and functions
AC_HEADER_STDC
AC_CHECK_HEADERS([unistd.h])
AC_CHECK_FUNCS([memset memmove strerror])

# Dependencies
GTK_REQUIRED_VERSION=2.12.0
LIBXML_REQUIRED_VERSION=2.5.0

AC_SUBST(GTK_REQUIRED_VERSION)
AC_SUBST(LIBXML_REQUIRED_VERSION)

# Pull glib-genmarshal & co.
AC_PATH_PROG(GLIB_GENMARSHAL, glib-genmarshal)
AC_PATH_PROG(GLIB_MKENUMS, glib-mkenums)

GNOME_COMPILE_WARNINGS(yes)

AC_ARG_ENABLE(deprecations,
              [AC_HELP_STRING([--enable-deprecations],
                              [warn about deprecated usages [default=no]])],,
              [enable_deprecations=no])

if test "x$enable_deprecations" = "xyes"; then
   DISABLE_DEPRECATED_CFLAGS="\
-DG_DISABLE_DEPRECATED \
-DGDK_DISABLE_DEPRECATED \
-DGTK_DISABLE_DEPRECATED \
-DGDK_PIXBUF_DISABLE_DEPRECATED"
   AC_SUBST(DISABLE_DEPRECATED_CFLAGS)
fi

PKG_CHECK_MODULES(DEP, [
	gio-2.0
	gtk+-2.0 >= $GTK_REQUIRED_VERSION
	libxml-2.0 >= $LIBXML_REQUIRED_VERSION
])


dnl Check to enable completion providers
AC_ARG_ENABLE(providers,
	    AC_HELP_STRING([--enable-completion-providers],
	    		   [Enable building completion providers (words)]),,
	    enable_providers=yes)

AM_CONDITIONAL(ENABLE_PROVIDERS, test "x$enable_providers" = "xyes")

dnl check for native osx
AC_MSG_CHECKING([for native Mac OS X])

gdk_windowing=`$PKG_CONFIG --variable=target gdk-2.0`

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

# i18N stuff
GETTEXT_PACKAGE=gtksourceview-2.0
AC_SUBST(GETTEXT_PACKAGE)
AC_DEFINE_UNQUOTED(GETTEXT_PACKAGE, "$GETTEXT_PACKAGE", [Package name for gettext])

AM_GLIB_GNU_GETTEXT

# test-widget uses this to find lang files and gtksourcebuffer.c 	 
ABS_TOP_SRCDIR=`cd $srcdir && pwd` 	 
AC_SUBST(ABS_TOP_SRCDIR)

# Output files
AC_CONFIG_FILES([
gtksourceview-2.0.pc
Makefile
gtksourceview-zip
po/Makefile.in
gtksourceview/Makefile
gtksourceview/completion-providers/Makefile
gtksourceview/completion-providers/words/Makefile
gtksourceview/language-specs/Makefile
docs/Makefile
docs/reference/Makefile
docs/reference/version.xml
tests/Makefile
])

AC_OUTPUT


echo "

Configuration:

	Source code location:	${srcdir}
	Compiler:		${CC}
"
