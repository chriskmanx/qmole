
collab_req="libxml-2.0 >= 2.4.0"
collab_xmpp_req="loudmouth-1.0 >= 1.0.1"
collab_sugar_req="dbus-glib-1 >= 0.70"
collab_service_req="libsoup-2.4"
collab_pkgs="$collab_req" 	# accumulate required packages

AC_ARG_ENABLE([collab-backend-fake], 
    [AS_HELP_STRING([--enable-collab-backend-fake], [Fake backend for debugging purposes only (default: off)])], 
[
	enable_collab_backend_fake=$enableval
], [
	enable_collab_backend_fake="no"
])

AC_ARG_ENABLE([collab-backend-xmpp], 
    [AS_HELP_STRING([--enable-collab-backend-xmpp], [Jabber backend (default: auto)])], 
[
	enable_collab_backend_xmpp=$enableval
], [
	PKG_CHECK_EXISTS([ $collab_xmpp_req ],
	[
		enable_collab_backend_xmpp="yes"
	])
])
test "$enable_collab_backend_xmpp" == "yes" && collab_pkgs="$collab_pkgs $collab_xmpp_req"

AC_ARG_ENABLE([collab-backend-tcp], 
    [AS_HELP_STRING([--enable-collab-backend-tcp], [TCP backend (default: auto)])], 
[
	enable_collab_backend_tcp=$enableval
	AC_LANG_PUSH(C++)
	AC_CHECK_HEADERS([asio.hpp], [], 
	[
		AC_MSG_ERROR([collab plugin: asio is required for the collab plugin TCP backend, see http://think-async.com/])
	])
	AC_LANG_POP
], [
	AC_LANG_PUSH(C++)
	AC_CHECK_HEADERS([asio.hpp], 
	[
		enable_collab_backend_tcp="yes"
	])
	AC_LANG_POP
])

AC_ARG_ENABLE([collab-backend-sugar], 
    [AS_HELP_STRING([--enable-collab-backend-sugar], [Sugar/OLPC backend (default: off)])], 
[
	enable_collab_backend_sugar=$enableval
], [])
test "$enable_collab_backend_sugar" == "yes" && collab_pkgs="$collab_pkgs $collab_sugar_req"

AC_ARG_ENABLE([collab-backend-service], 
    [AS_HELP_STRING([--enable-collab-backend-service], [abicollab.net backend (default: off); NOTE to packagers: do NOT enable this, the service is not publically available yet])], 
[
	enable_collab_backend_service=$enableval
	AC_LANG_PUSH(C++)
	AC_CHECK_HEADERS([asio.hpp], [], 
	[
		AC_MSG_ERROR([collab plugin: asio is required for the the abicollab.net backend, see http://think-async.com/])
	])
	AC_LANG_POP
], [
	AC_LANG_PUSH(C++)
	AC_CHECK_HEADERS([asio.hpp],
	[
		enable_collab_backend_service="yes"
	])
	AC_LANG_POP
])
test "$enable_collab_backend_service" == "yes" && collab_pkgs="$collab_pkgs $collab_service_req"

AC_ARG_ENABLE([collab-record-always], 
    [AS_HELP_STRING([--enable-collab-record-always], [Always record AbiCollab sessions (default: off)])], 
[
	enable_collab_record_always=$enableval
], [
	enable_collab_record_always="no"
])

collab_deps="no"

if test "$enable_collab" != ""; then

PKG_CHECK_EXISTS([ $collab_pkgs ], 
[
	collab_deps="yes"
])

fi

if test "$enable_collab" == "yes" || \
   test "$collab_deps" == "yes"; then

if test "$enable_collab_builtin" == "yes"; then
AC_MSG_ERROR([collab plugin: static linking not supported])
fi

# HACK, no way to detect, check only if explicitely enabled
if test "$enable_collab" == "yes"; then
# check for various boost libs, needs to be done before
AX_BOOST_BASE([1.33.1])
fi

PKG_CHECK_MODULES(COLLAB,[ $collab_pkgs ])

if test "$enable_collab_backend_fake" == "yes"; then
	COLLAB_CFLAGS="$COLLAB_CFLAGS -DABICOLLAB_HANDLER_FAKE"
fi
if test "$enable_collab_backend_xmpp" == "yes"; then
	COLLAB_CFLAGS="$COLLAB_CFLAGS -DABICOLLAB_HANDLER_XMPP"
fi
if test "$enable_collab_backend_tcp" == "yes"; then
	COLLAB_CFLAGS="$COLLAB_CFLAGS -DABICOLLAB_HANDLER_TCP"
fi
if test "$enable_collab_backend_sugar" == "yes"; then
	COLLAB_CFLAGS="$COLLAB_CFLAGS -DABICOLLAB_HANDLER_SUGAR"
fi
if test "$enable_collab_backend_service" == "yes"; then
	COLLAB_CFLAGS="$COLLAB_CFLAGS -DABICOLLAB_HANDLER_SERVICE -DSOUP24"
fi
if test "$enable_collab_record_always" == "yes"; then
	COLLAB_CFLAGS="$COLLAB_CFLAGS -DABICOLLAB_RECORD_ALWAYS"
fi

if test "$enable_collab_backend_tcp" == "yes" || \
   test "$enable_collab_backend_service" == "yes"; then
	COLLAB_LIBS="$COLLAB_LIBS -lpthread"
fi

test "$enable_collab" == "auto" && PLUGINS="$PLUGINS collab"

COLLAB_CFLAGS="$COLLAB_CFLAGS "'${PLUGIN_CFLAGS}'
COLLAB_LIBS="$COLLAB_LIBS "'${PLUGIN_LIBS}'

fi # plugin conditional

AM_CONDITIONAL([COLLAB_BACKEND_FAKE], [test "$enable_collab_backend_fake" == "yes"])
AM_CONDITIONAL([COLLAB_BACKEND_XMPP], [test "$enable_collab_backend_xmpp" == "yes"])
AM_CONDITIONAL([COLLAB_BACKEND_TCP], [test "$enable_collab_backend_tcp" == "yes"])
AM_CONDITIONAL([COLLAB_BACKEND_SUGAR], [test "$enable_collab_backend_sugar" == "yes"])
AM_CONDITIONAL([COLLAB_BACKEND_SERVICE], [test "$enable_collab_backend_service" == "yes"])
AM_CONDITIONAL([COLLAB_RECORD_ALWAYS], [test "$enable_collab_record_always" == "yes"])

AC_SUBST([COLLAB_CFLAGS])
AC_SUBST([COLLAB_LIBS])

