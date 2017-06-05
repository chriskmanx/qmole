
GOOGLE_CFLAGS=
GOOGLE_LIBS=

if test "$enable_google" != ""; then

test "$enable_google" == "auto" && PLUGINS="$PLUGINS google"

GOOGLE_CFLAGS="$GOOGLE_CFLAGS "'${PLUGIN_CFLAGS}'
GOOGLE_LIBS="$GOOGLE_LIBS "'${PLUGIN_LIBS}'

if test "$enable_google_builtin" == "yes"; then
	GOOGLE_CFLAGS="$GOOGLE_CFLAGS -DABI_PLUGIN_BUILTIN"
fi

fi

AC_SUBST([GOOGLE_CFLAGS])
AC_SUBST([GOOGLE_LIBS])

