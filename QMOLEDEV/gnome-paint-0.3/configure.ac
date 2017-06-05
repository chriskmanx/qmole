dnl Process this file with autoconf to produce a configure script.
dnl Created by Anjuta application wizard.

AC_INIT(gnome-paint, 0.3, http://code.google.com/p/gnome-paint/)

AM_INIT_AUTOMAKE(AC_PACKAGE_NAME, AC_PACKAGE_VERSION)
AC_CONFIG_HEADERS([config.h])
AM_MAINTAINER_MODE

AC_ISC_POSIX
AC_PROG_CC
AM_PROG_CC_STDC
AC_HEADER_STDC




dnl ***************************************************************************
dnl Internatinalization
dnl ***************************************************************************
IT_PROG_INTLTOOL([0.35.0])

GETTEXT_PACKAGE=gnome_paint
AC_SUBST(GETTEXT_PACKAGE)
AC_DEFINE_UNQUOTED(GETTEXT_PACKAGE,"$GETTEXT_PACKAGE", [GETTEXT package name])
AM_GLIB_GNU_GETTEXT




AM_PROG_LIBTOOL



PKG_CHECK_MODULES(GNOME_PAINT, [gtk+-2.0 >= 2.8])




AC_OUTPUT([
Makefile
src/Makefile
po/Makefile.in
data/Makefile
data/ui/Makefile
data/icons/Makefile
data/icons/16x16/Makefile
data/icons/16x16/actions/Makefile
data/icons/16x16/apps/Makefile
data/desktop/Makefile
data/desktop/gnome-paint.desktop.in
])
