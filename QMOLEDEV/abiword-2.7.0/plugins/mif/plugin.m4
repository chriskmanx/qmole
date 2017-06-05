
MIF_CFLAGS=
MIF_LIBS=

if test "$enable_mif" != ""; then

test "$enable_mif" == "auto" && PLUGINS="$PLUGINS mif"

MIF_CFLAGS="$MIF_CFLAGS "'${PLUGIN_CFLAGS}'
MIF_LIBS="$MIF_LIBS "'${PLUGIN_LIBS}'

if test "$enable_mif_builtin" == "yes"; then
	MIF_CFLAGS="$MIF_CFLAGS -DABI_PLUGIN_BUILTIN"
fi

fi

AC_SUBST([MIF_CFLAGS])
AC_SUBST([MIF_LIBS])

