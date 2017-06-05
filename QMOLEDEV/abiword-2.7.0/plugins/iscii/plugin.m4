
ISCII_CFLAGS=
ISCII_LIBS=

if test "$enable_iscii" != ""; then

test "$enable_iscii" == "auto" && PLUGINS="$PLUGINS iscii"

ISCII_CFLAGS="$ISCII_CFLAGS "'${PLUGIN_CFLAGS}'
ISCII_LIBS="$ISCII_LIBS "'${PLUGIN_LIBS}'

if test "$enable_iscii_builtin" == "yes"; then
	ISCII_CFLAGS="$ISCII_CFLAGS -DABI_PLUGIN_BUILTIN"
fi

fi

AC_SUBST([ISCII_CFLAGS])
AC_SUBST([ISCII_LIBS])

