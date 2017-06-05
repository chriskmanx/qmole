
hancom_pkgs="$gsf_req"
hancom_deps="no"

if test "$enable_hancom" != ""; then

PKG_CHECK_EXISTS([ $hancom_pkgs ], 
[
	hancom_deps="yes"
], [
	test "$enable_hancom" == "auto" && AC_MSG_WARN([hancom plugin: dependencies not satisfied - $hancom_pkgs])
])

fi

if test "$enable_hancom" == "yes" || \
   test "$hancom_deps" == "yes"; then

PKG_CHECK_MODULES(HANCOM,[ $hancom_pkgs ])

test "$enable_hancom" == "auto" && PLUGINS="$PLUGINS hancom"

HANCOM_CFLAGS="$HANCOM_CFLAGS "'${PLUGIN_CFLAGS}'
HANCOM_LIBS="$HANCOM_LIBS "'${PLUGIN_LIBS}'

if test "$enable_hancom_builtin" == "yes"; then
	HANCOM_CFLAGS="$HANCOM_CFLAGS -DABI_PLUGIN_BUILTIN"
fi

fi

AC_SUBST([HANCOM_CFLAGS])
AC_SUBST([HANCOM_LIBS])

