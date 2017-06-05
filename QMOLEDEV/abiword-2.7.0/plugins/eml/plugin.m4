
EML_CFLAGS=
EML_LIBS=

if test "$enable_eml" != ""; then

test "$enable_eml" == "auto" && PLUGINS="$PLUGINS eml"

EML_CFLAGS="$EML_CFLAGS "'${PLUGIN_CFLAGS}'
EML_LIBS="$EML_LIBS "'${PLUGIN_LIBS}'

if test "$enable_eml_builtin" == "yes"; then
	EML_CFLAGS="$EML_CFLAGS -DABI_PLUGIN_BUILTIN"
fi

fi

AC_SUBST([EML_CFLAGS])
AC_SUBST([EML_LIBS])

