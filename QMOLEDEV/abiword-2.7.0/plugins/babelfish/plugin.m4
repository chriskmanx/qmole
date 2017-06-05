
BABELFISH_CFLAGS=
BABELFISH_LIBS=

if test "$enable_babelfish" != ""; then

test "$enable_babelfish" == "auto" && PLUGINS="$PLUGINS babelfish"

BABELFISH_CFLAGS="$BABELFISH_CFLAGS "'${PLUGIN_CFLAGS}'
BABELFISH_LIBS="$BABELFISH_LIBS "'${PLUGIN_LIBS}'

if test "$enable_babelfish_builtin" == "yes"; then
	BABELFISH_CFLAGS="$BABELFISH_CFLAGS -DABI_PLUGIN_BUILTIN"
fi

fi

AC_SUBST([BABELFISH_CFLAGS])
AC_SUBST([BABELFISH_LIBS])

