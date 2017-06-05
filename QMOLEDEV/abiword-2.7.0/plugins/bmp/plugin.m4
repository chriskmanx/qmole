
BMP_CFLAGS=
BMP_LIBS=
bmp_deps="no"

if test "$enable_bmp" != ""; then

AC_MSG_CHECKING([for win32 toolkit])
if test "$TOOLKIT" == "win"; then
	AC_MSG_RESULT([yes])
	bmp_deps="yes"
else
	AC_MSG_RESULT([no])
	if test "$enable_bmp" == "auto"; then
	  AC_MSG_WARN([bmp plugin: only supported on win32])
	else
	  AC_MSG_ERROR([bmp plugin: only supported on win32])
	fi
fi

fi

if test "$enable_bmp" == "yes" || \
   test "$bmp_deps" == "yes"; then

# TODO check for libpng, well abiword links to it anyways

BMP_CFLAGS="$BMP_CFLAGS "'${PLUGIN_CFLAGS}'
BMP_LIBS="$BMP_LIBS "'${PLUGIN_LIBS} -lpng13'

if test "$enable_bmp_builtin" == "yes"; then
	BMP_CFLAGS="$BMP_CFLAGS -DABI_PLUGIN_BUILTIN"
fi

test "$enable_bmp" == "auto" && PLUGINS="$PLUGINS bmp"

fi

AC_SUBST([BMP_CFLAGS])
AC_SUBST([BMP_LIBS])

