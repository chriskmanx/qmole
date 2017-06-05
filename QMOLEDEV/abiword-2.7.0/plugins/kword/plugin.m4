
kword_pkgs="$gsf_req"
kword_deps="no"

if test "$enable_kword" != ""; then

PKG_CHECK_EXISTS([ $kword_pkgs ], 
[
	kword_deps="yes"
], [
	test "$enable_kword" == "auto" && AC_MSG_WARN([kword plugin: dependencies not satisfied - $kword_pkgs])
])

fi

if test "$enable_kword" == "yes" || \
   test "$kword_deps" == "yes"; then

PKG_CHECK_MODULES(KWORD,[ $kword_pkgs ])

test "$enable_kword" == "auto" && PLUGINS="$PLUGINS kword"

KWORD_CFLAGS="$KWORD_CFLAGS "'${PLUGIN_CFLAGS}'
KWORD_LIBS="$KWORD_LIBS "'${PLUGIN_LIBS}'

if test "$enable_kword_builtin" != ""; then
	KWORD_CFLAGS="$KWORD_CFLAGS -DABI_PLUGIN_BUILTIN"
fi

fi

AC_SUBST([KWORD_CFLAGS])
AC_SUBST([KWORD_LIBS])

