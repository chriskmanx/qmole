
loadbindings_pkgs="$gsf_req"
loadbindings_deps="no"

if test "$enable_loadbindings" != ""; then

PKG_CHECK_EXISTS([ $loadbindings_pkgs ], 
[
	loadbindings_deps="yes"
], [
	test "$enable_loadbindings" == "auto" && AC_MSG_WARN([loadbindings plugin: dependencies not satisfied - $loadbindings_pkgs])
])

fi

if test "$enable_loadbindings" == "yes" || \
   test "$loadbindings_deps" == "yes"; then

PKG_CHECK_MODULES(LOADBINDINGS,[ $loadbindings_pkgs ])

test "$enable_loadbindings" == "auto" && PLUGINS="$PLUGINS loadbindings"

LOADBINDINGS_CFLAGS="$LOADBINDINGS_CFLAGS "'${PLUGIN_CFLAGS}'
LOADBINDINGS_LIBS="$LOADBINDINGS_LIBS "'${PLUGIN_LIBS}'

if test "$enable_loadbindings_builtin" == "yes"; then
	LOADBINDINGS_CFLAGS="$LOADBINDINGS_CFLAGS -DABI_PLUGIN_BUILTIN"
fi

fi

AC_SUBST([LOADBINDINGS_CFLAGS])
AC_SUBST([LOADBINDINGS_LIBS])

