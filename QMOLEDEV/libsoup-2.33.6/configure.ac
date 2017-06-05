dnl *******************************************
dnl *** Initialize automake and set version ***
dnl *******************************************

AC_PREREQ(2.63)
AC_INIT([libsoup],[2.33.6],[http://bugzilla.gnome.org/enter_bug.cgi?product=libsoup])
AC_CONFIG_SRCDIR([libsoup-2.4.pc.in])
AC_CONFIG_MACRO_DIR([m4])

AM_INIT_AUTOMAKE([foreign 1.9])
m4_ifdef([AM_SILENT_RULES],[AM_SILENT_RULES([yes])],)

AC_PROG_MAKE_SET

SOUP_API_VERSION=2.4
AC_SUBST(SOUP_API_VERSION)

# Increment on interface addition. Reset on removal.
SOUP_AGE=3

# Increment on interface add, remove, or change.
SOUP_CURRENT=4

# Increment on source change. Reset when CURRENT changes.
SOUP_REVISION=0

AC_SUBST(SOUP_CURRENT)
AC_SUBST(SOUP_REVISION)
AC_SUBST(SOUP_AGE)

dnl ***************************
dnl *** Set debugging flags ***
dnl ***************************

debug_default=minimum

# Declare --enable-* args and collect ac_help strings
AC_ARG_ENABLE(debug, 
	      AS_HELP_STRING([--enable-debug=[no/minimum/yes]],
			     [turn on debugging (default=$debug_default)]),,
	      enable_debug=$debug_default)

# Set the debug flags
if test "x$enable_debug" = "xyes"; then
	test "$cflags_set" = set || CFLAGS="$CFLAGS -g"
	SOUP_DEBUG_FLAGS="-DG_ENABLE_DEBUG"
else
	if test "x$enable_debug" = "xno"; then
		SOUP_DEBUG_FLAGS="-DG_DISABLE_ASSERT -DG_DISABLE_CHECKS"
	fi
fi

AC_SUBST(SOUP_DEBUG_FLAGS)

# Set the maintainer flags
if test -d .git; then
	SOUP_MAINTAINER_FLAGS="-DG_DISABLE_DEPRECATED"
fi
AC_SUBST(SOUP_MAINTAINER_FLAGS)

dnl ***************************
dnl *** Checks for programs ***
dnl ***************************

AC_PROG_CC
AC_PROG_INSTALL

# Initialize libtool
LT_INIT([win32-dll])

dnl ***********************
dnl *** Checks for glib ***
dnl ***********************

AM_PATH_GLIB_2_0(2.27.5,,,gobject gthread gio)
if test "$GLIB_LIBS" = ""; then
   AC_MSG_ERROR(GLIB 2.27.5 or later is required to build libsoup)
fi
GLIB_CFLAGS="$GLIB_CFLAGS -DG_DISABLE_SINGLE_INCLUDES"

PKG_CHECK_MODULES(XML, libxml-2.0)
AC_SUBST(XML_CFLAGS)
AC_SUBST(XML_LIBS)

dnl ***********************
dnl *** Check for Win32 ***
dnl ***********************

AC_MSG_CHECKING([for Win32])
case "$host" in
  *-*-mingw*)
    os_win32=yes
    CFLAGS="$CFLAGS -D_REENTRANT"
    ;;
  *)
    os_win32=no
    ;;
esac
AC_MSG_RESULT([$os_win32])
AM_CONDITIONAL(OS_WIN32, [test $os_win32 = yes])

dnl *******************
dnl *** Misc checks ***
dnl *******************
AC_CHECK_FUNCS(gmtime_r)
AC_CHECK_FUNCS(mmap)
AC_CHECK_FUNC(socket, , AC_CHECK_LIB(socket, socket))

dnl *********************
dnl *** GNOME support ***
dnl *********************
AC_MSG_CHECKING(whether to build libsoup-gnome)
AC_ARG_WITH(gnome,
	    AS_HELP_STRING([--without-gnome], [Do not build libsoup-gnome]),
	    :, [if test $os_win32 = yes; then with_gnome=no; else with_gnome=yes; fi])
AC_MSG_RESULT($with_gnome)

if test $with_gnome != no; then
	PKG_CHECK_MODULES(GNOME_KEYRING, gnome-keyring-1, :,
			  AC_MSG_ERROR(
[Could not find gnome-keyring devel files.
Configure with --without-gnome if you wish to build only libsoup
without GNOME-specific features.]))
fi
AC_SUBST(GNOME_KEYRING_CFLAGS)
AC_SUBST(GNOME_KEYRING_LIBS)

AM_CONDITIONAL(BUILD_LIBSOUP_GNOME, test $with_gnome != no)

if test $with_gnome != no; then
	AC_DEFINE(HAVE_GNOME, 1, [Defined if GNOME support is enabled])

	PKG_CHECK_MODULES(LIBPROXY, libproxy-1.0, :, [AC_MSG_ERROR(dnl
[Could not find libproxy:

$LIBPROXY_PKG_ERRORS

This is required for GNOME proxy support.

Pass "--without-gnome" to configure if you want to build libsoup
without GNOME support.])])

	AC_MSG_CHECKING(libproxy version)
	libproxy_version=`$PKG_CONFIG --modversion libproxy-1.0`
	case $libproxy_version in
	0.2.*)
		AC_MSG_RESULT([$libproxy_version, using workarounds...])
		AC_DEFINE(HAVE_LIBPROXY_WITH_NON_THREAD_SAFE_GNOME_MODULE, 1, [Defined if libproxy is old and has a non-thread-safe gnome module])
		;;
	*)
		AC_MSG_RESULT([$libproxy_version, ok])
		;;
	esac

	PKG_CHECK_MODULES(GCONF, gconf-2.0, , [AC_MSG_ERROR(dnl
[Could not find GConf:

$GCONF_PKG_ERRORS

This is required for GNOME proxy support.

Pass "--without-gnome" to configure if you want to build libsoup
without GNOME support.])])

	PKG_CHECK_MODULES(SQLITE, sqlite3, :, [AC_MSG_ERROR(dnl
[Could not find sqlite3 devel files:

$SQLITE_PKG_ERRORS

Pass "--without-gnome" to configure if you want to build libsoup
without GNOME support.])])
fi
AC_SUBST(HAVE_GNOME)
AC_SUBST(GCONF_CFLAGS)
AC_SUBST(GCONF_LIBS)
AC_SUBST(LIBPROXY_CFLAGS)
AC_SUBST(LIBPROXY_LIBS)
AC_SUBST(SQLITE_CFLAGS)
AC_SUBST(SQLITE_LIBS)


dnl ***************
dnl *** gtk-doc ***
dnl ***************
GTK_DOC_CHECK([1.10])

dnl *****************************
dnl *** gobject-introspection ***
dnl *****************************
GOBJECT_INTROSPECTION_CHECK([0.9.5])

dnl *************************************
dnl *** Warnings to show if using GCC ***
dnl *************************************

AC_ARG_ENABLE(more-warnings,
	      AS_HELP_STRING([--disable-more-warnings], [Inhibit compiler warnings]),
	      set_more_warnings=no)

if test "$GCC" = "yes" -a "$set_more_warnings" != "no"; then
	CFLAGS="$CFLAGS \
		-Wall -Wstrict-prototypes -Wmissing-declarations \
		-Wmissing-prototypes -Wnested-externs -Wpointer-arith \
		-Wdeclaration-after-statement -Wformat=2 -Winit-self \
		-Waggregate-return -Wmissing-format-attribute"

	for option in -Wmissing-include-dirs -Wundef; do
		SAVE_CFLAGS="$CFLAGS"
		CFLAGS="$CFLAGS $option"
		AC_MSG_CHECKING([whether gcc understands $option])
		AC_COMPILE_IFELSE([AC_LANG_PROGRAM([], [])],
				  [has_option=yes],
				  [has_option=no])
		AC_MSG_RESULT($has_option)
		if test $has_option = no; then
			CFLAGS="$SAVE_CFLAGS"
		fi
	done
fi

dnl ******************************
dnl *** Stuff for regression tests
dnl ******************************
AC_MSG_NOTICE([checking for programs needed for regression tests])
MISSING_REGRESSION_TEST_PACKAGES=""

AC_MSG_CHECKING([for glib-networking (glib TLS implementation)])
save_CFLAGS="$CFLAGS"
save_LIBS="$LIBS"
CFLAGS="$CFLAGS $GLIB_CFLAGS"
LIBS="$LIBS $GLIB_LIBS"
AC_RUN_IFELSE([AC_LANG_PROGRAM([#include <gio/gio.h>],
			       [g_type_init (); return !g_tls_backend_supports_tls (g_tls_backend_get_default ());])],
	      [have_glib_networking=yes],
	      [have_glib_networking=no],
	      [have_glib_networking="unknown (cross-compiling)"])
CFLAGS="$save_CFLAGS"
LIBS="$save_LIBS"
AC_MSG_RESULT($have_glib_networking)
if test "$have_glib_networking" = "no"; then
        MISSING_REGRESSION_TEST_PACKAGES="$MISSING_REGRESSION_TEST_PACKAGES glib-networking"
fi

AC_ARG_WITH(apache-httpd,
	    AS_HELP_STRING([--with-apache-httpd], [Path to apache httpd (for tests)]),
	    APACHE_HTTPD="$withval",
	    [AC_PATH_PROGS(APACHE_HTTPD, httpd2 httpd apache2 apache, no, ${PATH}:/usr/sbin)])
if test "$APACHE_HTTPD" != "no"; then
    AC_MSG_CHECKING([Apache version])
    apache_version=`$APACHE_HTTPD -v 2>/dev/null | sed -ne 's/Server version: Apache\///p'`
    case $apache_version in
    2.2.*)
	AC_MSG_RESULT([$apache_version (ok)])
	;;
    *)
	AC_MSG_RESULT([$apache_version (ignoring)])
	APACHE_HTTPD="no"
	;;
    esac
fi
AC_SUBST(APACHE_HTTPD)
AC_DEFINE_UNQUOTED(APACHE_HTTPD, "$APACHE_HTTPD", [Apache httpd])

if test "$APACHE_HTTPD" != "no"; then
    AC_MSG_CHECKING([for Apache module directory])
    AC_ARG_WITH(apache-module-dir,
		AS_HELP_STRING([--with-apache-module-dir], [Apache modules dirs (for tests)]),
		apache_module_dirs="$withval",
		[apache_prefix=`dirname \`dirname $APACHE_HTTPD\``
		mpm=`$APACHE_HTTPD -V | sed -ne 's/^Server MPM: */-/p' | tr 'A-Z' 'a-z'`
		# This only works with bash, but should fail harmlessly in sh
		apache_module_dirs=`echo $apache_prefix/lib{64,}/{apache,apache2,http,http2,httpd}{$mpm,}{/modules,}`])
    for dir in $apache_module_dirs; do
        if test -f $dir/mod_auth_digest.so; then
	    APACHE_MODULE_DIR="$dir"
        fi
        if test -f $dir/mod_ssl.so; then
	    APACHE_SSL_MODULE_DIR="$dir"
	    IF_HAVE_APACHE_SSL=""
        fi
        if test -f $dir/mod_php5.so; then
	    APACHE_PHP_MODULE_DIR="$dir"
	    APACHE_PHP_MODULE=mod_php5.so
        fi
        if test -f $dir/libphp5.so; then
	    APACHE_PHP_MODULE_DIR="$dir"
	    APACHE_PHP_MODULE=libphp5.so
        fi
    done
    AC_MSG_RESULT($APACHE_MODULE_DIR)
    AC_SUBST(APACHE_MODULE_DIR)
    AC_SUBST(APACHE_SSL_MODULE_DIR)
    AC_SUBST(APACHE_PHP_MODULE_DIR)
    AC_SUBST(APACHE_PHP_MODULE)
fi

if test "$APACHE_HTTPD" != "no" -a -n "$APACHE_MODULE_DIR" -a -n "$APACHE_SSL_MODULE_DIR"; then
    AC_DEFINE(HAVE_APACHE, 1, [Whether or not apache can be used for tests])
    have_apache=1
    if test -z "$APACHE_PHP_MODULE_DIR"; then
        MISSING_REGRESSION_TEST_PACKAGES="$MISSING_REGRESSION_TEST_PACKAGES mod_php5"
    fi
else
    have_apache=0
    if test "$APACHE_HTTPD" = "no" -o -z "$APACHE_MODULE_DIR"; then
        MISSING_REGRESSION_TEST_PACKAGES="$MISSING_REGRESSION_TEST_PACKAGES apache"
    else
        MISSING_REGRESSION_TEST_PACKAGES="$MISSING_REGRESSION_TEST_PACKAGES mod_ssl"
    fi
fi
AM_CONDITIONAL(HAVE_APACHE, test $have_apache = 1)

if test "$have_apache" = 1; then
    AC_CHECK_PROGS(PHP, php php5)

    AC_MSG_CHECKING([for Apache PHP module])
    if test -f $APACHE_PHP_MODULE_DIR/$APACHE_PHP_MODULE; then
	have_php=yes
	IF_HAVE_PHP=""
    else
	have_php=no
	IF_HAVE_PHP="#"
	MISSING_REGRESSION_TEST_PACKAGES="$MISSING_REGRESSION_TEST_PACKAGES php5"
    fi
    AC_MSG_RESULT($have_php)

    if test "$have_php" = yes; then
	AC_MSG_CHECKING([for xmlrpc-epi-php])
	if $PHP --rf xmlrpc_server_create | grep -q "does not exist"; then
	    have_xmlrpc_epi_php=no
	    MISSING_REGRESSION_TEST_PACKAGES="$MISSING_REGRESSION_TEST_PACKAGES php-xmlrpc"
	else
	    have_xmlrpc_epi_php=yes
	fi
	AC_MSG_RESULT($have_xmlrpc_epi_php)
    fi
fi

AC_SUBST(IF_HAVE_PHP)
AM_CONDITIONAL(HAVE_XMLRPC_EPI_PHP, test "$have_xmlrpc_epi_php" = yes)

AC_PATH_PROG(CURL, curl, no)
if test "$CURL" != no; then
    AC_DEFINE(HAVE_CURL, 1, [Whether or not curl can be used for tests])
else
    MISSING_REGRESSION_TEST_PACKAGES="$MISSING_REGRESSION_TEST_PACKAGES curl"
fi
AM_CONDITIONAL(HAVE_CURL, test "$CURL" != no)

AC_SUBST(MISSING_REGRESSION_TEST_PACKAGES)
AM_CONDITIONAL(MISSING_REGRESSION_TEST_PACKAGES, test -n "$MISSING_REGRESSION_TEST_PACKAGES")

dnl *************************
dnl *** Output Everything ***
dnl *************************

AC_CONFIG_HEADERS([config.h])
AC_CONFIG_FILES([
	libsoup-2.4.pc
	libsoup-gnome-2.4.pc
	Makefile
	libsoup-zip
	libsoup/Makefile
	tests/Makefile
	tests/httpd.conf
	docs/Makefile
	docs/reference/Makefile
	])
AC_OUTPUT

if test -n "$MISSING_REGRESSION_TEST_PACKAGES"; then
    echo ""
    echo Some regression tests will not be run due to missing packages:
    echo $MISSING_REGRESSION_TEST_PACKAGES
    echo ""
fi
