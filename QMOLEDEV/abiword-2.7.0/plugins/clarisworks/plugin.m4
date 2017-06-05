
clarisworks_pkgs="$gsf_req"
clarisworks_deps="no"

if test "$enable_clarisworks" != ""; then

PKG_CHECK_EXISTS([ $clarisworks_pkgs ], 
[
	clarisworks_deps="yes"
], [
	test "$enable_clarisworks" == "auto" && AC_MSG_WARN([clarisworks plugin: dependencies not satisfied - $clarisworks_pkgs])
])

fi

if test "$enable_clarisworks" == "yes" || \
   test "$clarisworks_deps" == "yes"; then

PKG_CHECK_MODULES(CLARISWORKS,[ $clarisworks_pkgs ])

test "$enable_clarisworks" == "auto" && PLUGINS="$PLUGINS clarisworks"

CLARISWORKS_CFLAGS="$CLARISWORKS_CFLAGS "'${PLUGIN_CFLAGS}'
CLARISWORKS_LIBS="$CLARISWORKS_LIBS "'${PLUGIN_LIBS}'

if test "$enable_clarisworks_builtin" == "yes"; then
	CLARISWORKS_CFLAGS="$CLARISWORKS_CFLAGS -DABI_PLUGIN_BUILTIN"
fi

fi

AC_SUBST([CLARISWORKS_CFLAGS])
AC_SUBST([CLARISWORKS_LIBS])

