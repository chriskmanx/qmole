# $XdotOrg: $

AC_PREREQ([2.57])
AC_INIT(xcursor-themes, [1.0.1], [https://bugs.freedesktop.org/enter_bug.cgi?product=xorg], xcursor-themes)
AM_INIT_AUTOMAKE([dist-bzip2])
AM_MAINTAINER_MODE

AM_CONFIG_HEADER(config.h)

AC_PROG_CC
AC_PROG_INSTALL

AC_TYPE_SIGNAL

AC_PATH_PROG(XCURSORGEN, xcursorgen, no)
if test "x$XCURSORGEN" = xno; then
	AC_MSG_ERROR([xcursorgen is required to build these cursors.])
fi
AC_SUBST([XCURSORGEN])

PKG_CHECK_MODULES(ICONDEFS, xcursor)
cursordir=$(pkg-config --variable=icondir xcursor)
AC_SUBST([cursordir])

AM_CONDITIONAL(WHITEGLASS, true)
AM_CONDITIONAL(REDGLASS, true)
AM_CONDITIONAL(HANDHELDS, true)

XORG_RELEASE_VERSION

AC_OUTPUT([
	Makefile
	handhelds/Makefile
	redglass/Makefile
	whiteglass/Makefile
])
