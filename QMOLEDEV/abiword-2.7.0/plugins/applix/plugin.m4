
applix_pkgs="$gsf_req"
applix_deps="no"

if test "$enable_applix" != ""; then

PKG_CHECK_EXISTS([ $applix_pkgs ], 
[
	applix_deps="yes"
], [
	test "$enable_applix" == "auto" && AC_MSG_WARN([applix plugin: dependencies not satisfied - $applix_pkgs])
])

fi

if test "$enable_applix" == "yes" || \
   test "$applix_deps" == "yes"; then

PKG_CHECK_MODULES(APPLIX,[ $applix_pkgs ])

test "$enable_applix" == "auto" && PLUGINS="$PLUGINS applix"

APPLIX_CFLAGS="$APPLIX_CFLAGS "'${PLUGIN_CFLAGS}'
APPLIX_LIBS="$APPLIX_LIBS "'${PLUGIN_LIBS}'

if test "$enable_applix_builtin" == "yes"; then
	APPLIX_CFLAGS="$APPLIX_CFLAGS -DABI_PLUGIN_BUILTIN"
fi

fi

AC_SUBST([APPLIX_CFLAGS])
AC_SUBST([APPLIX_LIBS])

