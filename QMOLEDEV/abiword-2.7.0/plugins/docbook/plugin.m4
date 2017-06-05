
docbook_pkgs="$gsf_req"
docbook_deps="no"

if test "$enable_docbook" != ""; then

PKG_CHECK_EXISTS([ $docbook_pkgs ], 
[
	docbook_deps="yes"
], [
	test "$enable_docbook" == "auto" && AC_MSG_WARN([docbook plugin: dependencies not satisfied - $docbook_pkgs])
])

fi

if test "$enable_docbook" == "yes" || \
   test "$docbook_deps" == "yes"; then

AC_HEADER_TIME

PKG_CHECK_MODULES(DOCBOOK,[ $docbook_pkgs ])

test "$enable_docbook" == "auto" && PLUGINS="$PLUGINS docbook"

DOCBOOK_CFLAGS="$DOCBOOK_CFLAGS "'${PLUGIN_CFLAGS}'
DOCBOOK_LIBS="$DOCBOOK_LIBS "'${PLUGIN_LIBS}'

if test "$enable_docbook_builtin" == "yes"; then
	DOCBOOK_CFLAGS="$DOCBOOK_CFLAGS -DABI_PLUGIN_BUILTIN"
fi

fi

AC_SUBST([DOCBOOK_CFLAGS])
AC_SUBST([DOCBOOK_LIBS])

