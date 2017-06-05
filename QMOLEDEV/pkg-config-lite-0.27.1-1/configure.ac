AC_PREREQ([2.62])
AC_INIT([pkg-config],
        [0.27.1],
        [https://bugs.freedesktop.org/enter_bug.cgi?product=pkg-config],
        [pkg-config])

AC_CONFIG_HEADERS([config.h])
AC_CONFIG_SRCDIR([main.c])

dnl Initialize automake and libtool
AM_INIT_AUTOMAKE([1.11])
LT_PREREQ([2.2])
LT_INIT

dnl Support silent build rules, requires at least automake-1.11. Disable
dnl by either passing --disable-silent-rules to configure or passing V=1
dnl to make
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])

dnl Check for programs
AC_PROG_CC

dnl Check for headers
AC_CHECK_HEADERS([dirent.h unistd.h sys/wait.h malloc.h])

dnl A POSIX shell is required for the tests. If TEST_SHELL hasn't been
dnl set on the command line then we try to find bash or ksh or sh from
dnl the path. If none of those are available, we just use whatever
dnl autoconf detected for configure.
AC_ARG_VAR([TESTS_SHELL], [Path to a POSIX shell to be used for testing])
conf_path="$PATH:`getconf PATH 2>/dev/null`"
AC_PATH_PROGS([TESTS_SHELL], [bash ksh sh], [$CONFIG_SHELL], [$conf_path])

dnl
dnl Default pkg-config search path
dnl
AC_MSG_CHECKING([for default search path for .pc files])
AC_ARG_WITH([pc_path],
  [AS_HELP_STRING([--with-pc-path],
    [default search path for .pc files])],
  [pc_path="$withval"],
  [
# This is slightly wrong, but hopefully causes less confusion than
# people being unable to find their .pc files in the standard location.
if test "${prefix}" = "NONE"; then
	pc_path='${libdir}/pkgconfig:${datadir}/pkgconfig:/usr/lib/pkgconfig:/usr/share/pkgconfig'
else
	pc_path='${libdir}/pkgconfig:${datadir}/pkgconfig'
fi
])
AC_MSG_RESULT([$pc_path])
AC_SUBST([pc_path])

dnl
dnl System default -I paths
dnl
AC_MSG_CHECKING([for system include path to avoid -I flags])
AC_ARG_WITH([system_include_path],
  [AS_HELP_STRING([--with-system-include-path],
    [avoid -I flags from the given path])],
  [system_include_path="$withval"],
  [system_include_path="/usr/include"])
AC_MSG_RESULT([$system_include_path])
AC_SUBST([system_include_path])

dnl
dnl System default -L paths
dnl
AC_MSG_CHECKING([for system library path to avoid -L flags])
AC_ARG_WITH([system_library_path],
  [AS_HELP_STRING([--with-system-library-path],
    [avoid -L flags from the given path])],
  [system_library_path="$withval"],
  [
case "$libdir" in
*lib64)
  system_library_path="/usr/lib64:/usr/lib"
  ;;
*)
  system_library_path="/usr/lib"
  ;;
esac
])
AC_MSG_RESULT([$system_library_path])
AC_SUBST([system_library_path])

dnl Code taken from gtk+-2.0's configure.in.
dnl
dnl This causes pkg-config to not list private dependencies (a very
dnl common example is libm) on platforms which support inter-library
dnl dependencies.
dnl
dnl When using libtool 2.x, we need to force libtool to be created
dnl early so we can use it here.
m4_ifdef([LT_OUTPUT], [LT_OUTPUT])

AC_ARG_ENABLE([indirect-deps],
    [AS_HELP_STRING([--enable-indirect-deps],
                    [list both direct and indirect dependencies.])
     AS_HELP_STRING([--disable-indirect-deps],
                    [only list direct dependencies.])
    [default=auto]],
    [use_indirect_deps=$enableval],
    [use_indirect_deps=auto])
echo use_indirect_deps=$use_indirect_deps >&AS_MESSAGE_LOG_FD
AC_MSG_CHECKING([whether to list both direct and indirect dependencies])
case $use_indirect_deps in
  auto)
    deplibs_check_method=`(./libtool --config; echo eval echo '\"$deplibs_check_method\"') | sh`
    if test "X$deplibs_check_method" = Xnone; then
    echo "foo: $deplibs_check_method" >&AS_MESSAGE_LOG_FD
      use_indirect_deps=yes
    else
      use_indirect_deps=no
    fi
  ;;
  yes|no)
  ;;
  *) AC_MSG_ERROR([Value given to --enable-indirect-deps must be one of yes, no
or auto])
  ;;
esac
AC_MSG_RESULT($use_indirect_deps)
AC_DEFINE_UNQUOTED([ENABLE_INDIRECT_DEPS],
    [`test $use_indirect_deps = no; echo $?`],
    [Link library to all dependent libraries, not only directly needed ones])
AC_SUBST([use_indirect_deps])

dnl
dnl Choose default CFLAGS and warnings depending on compiler.
dnl
WARN_CFLAGS=""
if test "${GCC}" = "yes" && test "${ac_env_CFLAGS_set}" != "set"; then
   WARN_CFLAGS="-g -Wall -O2"
fi
AC_SUBST(WARN_CFLAGS)

AC_MSG_CHECKING([for Win32])
case "$build" in
  *-*-mingw*)
    native_win32=yes
  ;;
  *)
    native_win32=no
  ;;
esac
AC_MSG_RESULT([$native_win32])
AM_CONDITIONAL(NATIVE_WIN32, [test "x$native_win32" = xyes])

# dnl
# dnl Find glib or use internal copy. Required version is 2.16 for
# dnl g_win32_get_package_installation_directory_of_module().
# dnl
# m4_define([glib_module], ["glib-2.0 >= 2.16"])
# AC_ARG_WITH([internal-glib],
#   [AS_HELP_STRING([--with-internal-glib], [use internal glib])],
#   [with_internal_glib="$withval"],
#   [with_internal_glib=no])
# AM_CONDITIONAL([INTERNAL_GLIB], [test "x$with_internal_glib" = xyes])
# AC_MSG_CHECKING([if internal glib should be used])
# if test "x$with_internal_glib" = xyes; then
#   AC_MSG_RESULT([yes])
#   GLIB_CFLAGS='-I$(top_srcdir)/glib -I$(top_srcdir)/glib/glib \
#     -I$(top_builddir)/glib/glib'
#   GLIB_LIBS='$(top_builddir)/glib/glib/libglib-2.0.la'
#   AC_CONFIG_SUBDIRS([glib])
# else
#   AC_MSG_RESULT([no])
#   if test "x$GLIB_CFLAGS" = "x" && test "x$GLIB_LIBS" = "x"; then
#     AC_CHECK_PROGS([PKG_CONFIG], [pkg-config], [])
#     if test -n $PKG_CONFIG && $PKG_CONFIG --exists glib_module; then
#       GLIB_CFLAGS=`$PKG_CONFIG --cflags glib_module`
#       GLIB_LIBS=`$PKG_CONFIG --libs glib_module`
#     else
#       AC_MSG_ERROR(m4_normalize([pkg-config and ]glib_module[ not found,
#                                  please set GLIB_CFLAGS and GLIB_LIBS to
#                                  the correct values or pass
#                                  --with-internal-glib to configure]))
#     fi
#   fi
# fi
GLIB_LIBS=''
GLIB_CFLAGS=''
AC_SUBST(GLIB_LIBS)
AC_SUBST(GLIB_CFLAGS)

AC_CONFIG_FILES([
Makefile
check/Makefile
check/config.sh
])
AC_OUTPUT
