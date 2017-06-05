
HRTEXT_CFLAGS=
HRTEXT_LIBS=

if test "$enable_hrtext" != ""; then

test "$enable_hrtext" == "auto" && PLUGINS="$PLUGINS hrtext"

HRTEXT_CFLAGS="$HRTEXT_CFLAGS "'${PLUGIN_CFLAGS}'
HRTEXT_LIBS="$HRTEXT_LIBS "'${PLUGIN_LIBS}'

if test "$enable_hrtext_builtin" == "yes"; then
	HRTEXT_CFLAGS="$HRTEXT_CFLAGS -DABI_PLUGIN_BUILTIN"
fi

fi

AC_SUBST([HRTEXT_CFLAGS])
AC_SUBST([HRTEXT_LIBS])

