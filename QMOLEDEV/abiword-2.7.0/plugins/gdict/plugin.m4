
GDICT_CFLAGS=
GDICT_LIBS=
gdict_deps="no"

if test "$enable_gdict" != ""; then

AC_MSG_CHECKING([for unix/gtk platform])
if test "$TOOLKIT" == "gtk"; then
  AC_MSG_RESULT([yes])
  gdict_deps="yes"
else
  AC_MSG_RESULT([no])
  if test "$enable_gdict" == "auto"; then
    AC_MSG_WARN([gdict plugin: only supported on UNIX/gtk platforms])
  else
    AC_MSG_ERROR([gdict plugin: only supported on UNIX/gtk platforms])
  fi
fi

fi

if test "$enable_gdict" == "yes" || \
   test "$gdict_deps" == "yes"; then

AC_TYPE_PID_T

test "$enable_gdict" == "auto" && PLUGINS="$PLUGINS gdict"

GDICT_CFLAGS="$GDICT_CFLAGS "'${PLUGIN_CFLAGS} -DUSE_FORK_AND_EXEC_METHOD=1'
GDICT_LIBS='${PLUGIN_LIBS}'

if test "$enable_gdict_builtin" != ""; then
	GDICT_CFLAGS="$GDICT_CFLAGS -DABI_PLUGIN_BUILTIN"
fi

fi

AC_SUBST([GDICT_CFLAGS])
AC_SUBST([GDICT_LIBS])

