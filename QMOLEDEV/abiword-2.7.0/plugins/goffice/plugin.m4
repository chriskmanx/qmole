
goffice_req=
for ver in 0.8 ; do
  if test "x$goffice_req" = x; then
    if pkg-config --exists libgoffice-$ver; then
      goffice_req=libgoffice-$ver
    fi
  fi
done
if test "x$goffice_req" = x; then
  goffice_req=libgoffice-0.8
fi

goffice_pkgs="$goffice_req >= 0.7.6"
goffice_deps="no"

if test "$enable_goffice" != ""; then

PKG_CHECK_EXISTS([ $goffice_pkgs ], 
[
	AC_MSG_CHECKING([for gtk toolkit])
	if test "$TOOLKIT" == "gtk"; then
	  AC_MSG_RESULT([yes])
	  goffice_deps="yes"
	else
	  AC_MSG_RESULT([no])
	  if test "$enable_goffice" == "auto"; then
	    AC_MSG_WARN([goffice plugin: only supported with gtk])
	  else
	    AC_MSG_ERROR([goffice plugin: only supported with gtk])
	  fi
	fi
], [
	test "$enable_goffice" == "auto" && AC_MSG_WARN([goffice plugin: dependencies not satisfied - $goffice_pkgs])
])

fi

if test "$enable_goffice" == "yes" || \
   test "$goffice_deps" == "yes"; then

if test "$enable_goffice_builtin" == "yes"; then
AC_MSG_ERROR([goffice plugin: static linking not supported])
fi

PKG_CHECK_MODULES(GOFFICE,[ $goffice_pkgs ])

test "$enable_goffice" == "auto" && PLUGINS="$PLUGINS goffice"

GOFFICE_CFLAGS="$GOFFICE_CFLAGS "'${PLUGIN_CFLAGS}'
GOFFICE_LIBS="$GOFFICE_LIBS "'${PLUGIN_LIBS}'

fi

AC_SUBST([GOFFICE_CFLAGS])
AC_SUBST([GOFFICE_LIBS])

