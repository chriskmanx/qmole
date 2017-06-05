
JPEG_CFLAGS=
JPEG_LIBS=
jpeg_deps="no"

if test "$enable_jpeg" != ""; then

AC_MSG_CHECKING([for win32 toolkit])
if test "$TOOLKIT" == "win"; then
	AC_MSG_RESULT([yes])
	AC_CHECK_HEADER(jpeglib.h,[
		AC_CHECK_LIB(jpeg,jpeg_read_header,
		[
			jpeg_deps="yes"
		], [
			if test "$enable_jpeg" == "auto"; then
			  AC_MSG_WARN([jpeg: libjpeg not found])
			else
			  AC_MSG_ERROR([jpeg: libjpeg not found])
			fi
		])
	],[
		if test "$enable_jpeg" == "auto"; then
		  AC_MSG_WARN([jpeg: jpeg header not found])
		else
		  AC_MSG_ERROR([jpeg: jpeg header not found])
		fi
	])
else
	AC_MSG_RESULT([no])
	if test "$enable_jpeg" == "auto"; then
	  AC_MSG_WARN([jpeg plugin: only supported on win32])
	else
	  AC_MSG_ERROR([jpeg plugin: only supported on win32])
	fi
fi

fi

if test "$enable_jpeg" == "yes" || \
   test "$jpeg_deps" == "yes"; then

test "$enable_jpeg" == "auto" && PLUGINS="$PLUGINS jpeg"

JPEG_CFLAGS='${PLUGIN_CFLAGS}'
JPEG_LIBS="-ljpeg "'${PLUGIN_LIBS}'

if test "$enable_jpeg_builtin" == "yes"; then
	JPEG_CFLAGS="$JPEG_CFLAGS -DABI_PLUGIN_BUILTIN"
fi

fi

AC_SUBST([JPEG_CFLAGS])
AC_SUBST([JPEG_LIBS])

