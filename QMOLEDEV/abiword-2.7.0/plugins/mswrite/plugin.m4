
mswrite_pkgs="$gsf_req"
mswrite_deps="no"

if test "$enable_mswrite" != ""; then

PKG_CHECK_EXISTS([ $mswrite_pkgs ], 
[
	mswrite_deps="yes"
], [
	test "$enable_mswrite" == "auto" && AC_MSG_WARN([mswrite plugin: dependencies not satisfied - $mswrite_pkgs])
])

fi

if test "$enable_mswrite" == "yes" || \
   test "$mswrite_deps" == "yes"; then

PKG_CHECK_MODULES(MSWRITE,[ $mswrite_pkgs ])

test "$enable_mswrite" == "auto" && PLUGINS="$PLUGINS mswrite"

MSWRITE_CFLAGS="$MSWRITE_CFLAGS "'${PLUGIN_CFLAGS}'
MSWRITE_LIBS="$MSWRITE_LIBS "'${PLUGIN_LIBS}'

if test "$enable_mswrite_builtin" == "yes"; then
	MSWRITE_CFLAGS="$MSWRITE_CFLAGS -DABI_PLUGIN_BUILTIN"
fi

fi

AC_SUBST([MSWRITE_CFLAGS])
AC_SUBST([MSWRITE_LIBS])

