
NROFF_CFLAGS=
NROFF_LIBS=

if test "$enable_nroff" != ""; then

test "$enable_nroff" == "auto" && PLUGINS="$PLUGINS nroff"

NROFF_CFLAGS="$NROFF_CFLAGS "'${PLUGIN_CFLAGS}'
NROFF_LIBS="$NROFF_LIBS "'${PLUGIN_LIBS}'

if test "$enable_nroff_builtin" == "yes"; then
	NROFF_CFLAGS="$NROFF_CFLAGS -DABI_PLUGIN_BUILTIN"
fi

fi

AC_SUBST([NROFF_CFLAGS])
AC_SUBST([NROFF_LIBS])

