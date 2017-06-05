dnl Process this file with autoconf to produce a configure script.

AC_PREREQ(2.54)

# Making releases on the stable branch:
#   BABL_MICRO_VERSION += 1;
#   BABL_INTERFACE_AGE += 1;
#   BABL_BINARY_AGE += 1;
# if any functions have been added,
#    set BABL_INTERFACE_AGE to 0.
# if backwards compatibility has been broken,
#    set BABL_BINARY_AGE _and_ BABL_INTERFACE_AGE to 0.


m4_define([babl_major_version], [0])
m4_define([babl_minor_version], [1])
m4_define([babl_micro_version], [4])
m4_define([babl_real_version],
          [babl_major_version.babl_minor_version.babl_micro_version])
m4_define([babl_version], [babl_real_version])

dnl Number of releases since we've added interfaces
m4_define([babl_interface_age], [1])
m4_define([babl_binary_age],
          [m4_eval(100 * babl_minor_version + babl_micro_version)])

# This is the X.Y used in -lbabl-X.Y
m4_define([babl_api_version], [babl_major_version.babl_minor_version])

m4_define([stability_version_number],
          m4_if(m4_eval(babl_major_version || babl_minor_version), [0],
                        [babl_micro_version], [babl_minor_version]))
m4_define([babl_unstable],
          m4_if(m4_eval(stability_version_number % 2), [1], [yes], [no]))
m4_define([babl_stable],
          m4_if(m4_eval(stability_version_number % 2), [0], [yes], [no]))

AC_INIT(babl, babl_major_version.babl_minor_version.babl_micro_version)
AC_CONFIG_SRCDIR(babl/babl.h)
AC_CONFIG_MACRO_DIR([m4])

# Enable silent build rules by default, requires at least
# Automake-1.11. Disable by either passing --disable-silent-rules to
# configure or passing V=1 to make
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])

BABL_MAJOR_VERSION=babl_major_version
BABL_MINOR_VERSION=babl_minor_version
BABL_MICRO_VERSION=babl_micro_version
BABL_INTERFACE_AGE=babl_interface_age
BABL_BINARY_AGE=babl_binary_age
BABL_VERSION=babl_version
BABL_REAL_VERSION=babl_real_version
BABL_API_VERSION=babl_api_version
AC_SUBST(BABL_MAJOR_VERSION)
AC_SUBST(BABL_MINOR_VERSION)
AC_SUBST(BABL_MICRO_VERSION)
AC_SUBST(BABL_INTERFACE_AGE)
AC_SUBST(BABL_BINARY_AGE)
AC_SUBST(BABL_VERSION)
AC_SUBST(BABL_REAL_VERSION)
AC_SUBST(BABL_API_VERSION)

BABL_RELEASE=babl_api_version
AC_SUBST(BABL_RELEASE)

# GObject Introspection
GOBJECT_INTROSPECTION_CHECK([0.6.8])

dnl The symbol BABL_UNSTABLE is defined above for substitution in
dnl Makefiles and conditionally defined here as a preprocessor symbol
dnl and automake conditional.
BABL_UNSTABLE=babl_unstable 
if test "x$BABL_UNSTABLE" = "xyes"; then
  AC_DEFINE(BABL_UNSTABLE, 1,
            [Define to 1 if this is an unstable version of BABL])
fi
AM_CONDITIONAL(BABL_UNSTABLE, test "x$BABL_UNSTABLE" = "xyes")
AC_SUBST(BABL_UNSTABLE)


# libtool versioning
m4_define([lt_current], [m4_eval(100 * babl_minor_version + babl_micro_version - babl_interface_age)])
m4_define([lt_revision], [babl_interface_age])
m4_define([lt_age], [m4_eval(babl_binary_age - babl_interface_age)])
BABL_LIBRARY_VERSION="lt_current:lt_revision:lt_age"
BABL_CURRENT_MINUS_AGE=m4_eval(lt_current - lt_age)
AC_SUBST(BABL_LIBRARY_VERSION)
AC_SUBST(BABL_CURRENT_MINUS_AGE)

PACKAGE_NAME=babl
AC_SUBST(PACKAGE_NAME)


dnl ==========================================================================

AM_INIT_AUTOMAKE(no-define no-dist-gzip)
AM_CONFIG_HEADER(config.h)

BABL_VARIADIC_MACROS

AC_DISABLE_STATIC
AC_PROG_CC
AC_LIBTOOL_WIN32_DLL
AM_PROG_LIBTOOL

dnl Initialize maintainer mode
AM_MAINTAINER_MODE

AC_STDC_HEADERS
AM_SANITY_CHECK

WEBSITE_HOST=pippin.gimp.org
AC_SUBST(WEBSITE_HOST)
WEBSITE_LOCATION=public_html/babl/
AC_SUBST(WEBSITE_LOCATION)


if eval "test x$GCC = xyes"; then
  case " $CFLAGS " in
  *[\ \	]-Wall[\ \	]*) ;;
  *) CFLAGS="$CFLAGS -Wall" ;;
  esac

BABL_DETECT_CFLAGS(extra_warnings, '-Wdeclaration-after-statement')
CFLAGS="$CFLAGS $extra_warnings"

BABL_DETECT_CFLAGS(extra_warnings, '-Wmissing-prototypes')
CFLAGS="$CFLAGS $extra_warnings"

BABL_DETECT_CFLAGS(extra_warnings, '-Wmissing-declarations')
CFLAGS="$CFLAGS $extra_warnings"

BABL_DETECT_CFLAGS(extra_warnings, '-Winit-self')
CFLAGS="$CFLAGS $extra_warnings"

BABL_DETECT_CFLAGS(extra_warnings, '-Wpointer-arith')
CFLAGS="$CFLAGS $extra_warnings"

BABL_DETECT_CFLAGS(extra_warnings, '-Wold-style-definition')
CFLAGS="$CFLAGS $extra_warnings"

fi

AC_PATH_PROG(RSVG, rsvg, no)
AM_CONDITIONAL(HAVE_RSVG, test "x$RSVG" != "xno")

AC_PATH_PROG(W3M, w3m, no)
AM_CONDITIONAL(HAVE_W3M, test "x$W3M" != "xno")

###########################
# Check target architecture
###########################

AC_MSG_CHECKING([for target architecture])
case x"$target" in
  xNONE | x)
    target_or_host="$host" ;;
  *)
    target_or_host="$target" ;;
esac
AC_MSG_RESULT([$target_or_host])

case "$target_or_host" in
  i*86-*-*)
    have_x86=yes
    AC_DEFINE(ARCH_X86, 1, [Define to 1 if you are compiling for ix86.])
    ;;
  x86_64-*-*)
    have_x86=yes
    AC_DEFINE(ARCH_X86, 1, [Define to 1 if you are compiling for ix86.])
    AC_DEFINE(ARCH_X86_64, 1, [Define to 1 if you are compiling for amd64.])
    ;;
  ppc-*-* | powerpc-*)
    have_ppc=yes
    AC_DEFINE(ARCH_PPC, 1, [Define to 1 if you are compiling for PowerPC.])
    ;;
  ppc64-*-* | powerpc64-*)
    have_ppc=yes
    AC_DEFINE(ARCH_PPC, 1, [Define to 1 if you are compiling for PowerPC.])
    AC_DEFINE(ARCH_PPC64, 1, [Define to 1 if you are compiling for PowerPC64.])
    ;;
  *)
    ;;
esac


############################
# Check how to find plug-ins
############################

AC_MSG_CHECKING([for some Win32 platform])
case "$target_or_host" in 
  *-*-darwin*)                 # darwin
    shrext=.dylib
    ;;
  hppa*-hpux*)		       # HP/UX
    shrext=.sl
    ;;
  *-*-mingw* | *-*-cygwin*)    # windows
    shrext=.dll
    ;;
  *)                           # linux (and BSD?)
    shrext=.so
    ;;
esac

SHREXT=$shrext
AC_SUBST(SHREXT)
AC_DEFINE_UNQUOTED(SHREXT,     "$shrext",     [File extension for shared libraries])

dnl ===========================================================================

#################
# Check for Win32
#################

AC_MSG_CHECKING([for some Win32 platform])
case "$target_or_host" in 
  *-*-mingw* | *-*-cygwin*)
    platform_win32=yes
    ;;
  *)
    platform_win32=no
    ;;
esac
AC_MSG_RESULT([$platform_win32])
AM_CONDITIONAL(PLATFORM_WIN32, test "$platform_win32" = "yes")

MATH_LIB=-lm
AC_MSG_CHECKING([for native Win32])
case "$target_or_host" in
  *-*-mingw*)
    os_win32=yes
    PATH_SEP=';'
    DIR_SEP='\\'
    MATH_LIB=
    ;;
  *)
    os_win32=no
    PATH_SEP=':'
    DIR_SEP='/'
    ;;
esac
AC_MSG_RESULT([$os_win32])
AC_SUBST(PATH_SEP)
AC_SUBST(DIR_SEP)
AC_SUBST(MATH_LIB)

AM_CONDITIONAL(OS_WIN32, test "$os_win32" = "yes")
AM_CONDITIONAL(OS_UNIX, test "$os_win32" != "yes")


dnl ===========================================================================


########################
# Check for MMX assembly
########################

AC_ARG_ENABLE(mmx,
  [  --enable-mmx            enable MMX support (default=auto)],,
  enable_mmx=$have_x86)

AC_ARG_ENABLE(sse,
  [  --enable-sse            enable SSE support (default=auto)],,
  enable_sse=$enable_mmx)

if test "x$enable_mmx" = xyes; then
  BABL_DETECT_CFLAGS(MMX_EXTRA_CFLAGS, '-mmmx')
  SSE_EXTRA_CFLAGS=

  AC_MSG_CHECKING(whether we can compile MMX code)

  mmx_save_CFLAGS="$CFLAGS"
  CFLAGS="$mmx_save_CFLAGS $MMX_EXTRA_CFLAGS"

  AC_COMPILE_IFELSE([AC_LANG_PROGRAM(,[asm ("movq 0, %mm0");])],

    AC_DEFINE(USE_MMX, 1, [Define to 1 if MMX assembly is available.])
    AC_MSG_RESULT(yes)

    if test "x$enable_sse" = xyes; then
      BABL_DETECT_CFLAGS(sse_flag, '-msse')
      SSE_EXTRA_CFLAGS="$MMX_EXTRA_CFLAGS $sse_flag"

      AC_MSG_CHECKING(whether we can compile SSE code)

      CFLAGS="$CFLAGS $sse_flag"

      AC_COMPILE_IFELSE([AC_LANG_PROGRAM(,[asm ("movntps %xmm0, 0");])],
        AC_DEFINE(USE_SSE, 1, [Define to 1 if SSE assembly is available.])
        AC_MSG_RESULT(yes)
      ,
        enable_sse=no
        AC_MSG_RESULT(no)
        AC_MSG_WARN([The assembler does not support the SSE command set.])
      )

    fi
  ,
    enable_mmx=no
    AC_MSG_RESULT(no)
    AC_MSG_WARN([The assembler does not support the MMX command set.])
  )

  CFLAGS="$mmx_save_CFLAGS"

  AC_SUBST(MMX_EXTRA_CFLAGS)
  AC_SUBST(SSE_EXTRA_CFLAGS)
fi


############################
# Check for AltiVec assembly
############################

AC_ARG_ENABLE(altivec,
  [  --enable-altivec        enable AltiVec support (default=auto)],,
  enable_altivec=$have_ppc)

if test "x$enable_altivec" = xyes; then

  BABL_DETECT_CFLAGS(altivec_flag, '-faltivec' '-maltivec -mabi=altivec')

  ALTIVEC_EXTRA_CFLAGS=
  case "$altivec_flag" in
    -maltivec*)
      altivec_save_CPPFLAGS="$CPPFLAGS"
      CPPFLAGS="$altivec_save_CPPFLAGS $altivec_flag"
      AC_CHECK_HEADERS(altivec.h, [ALTIVEC_EXTRA_CFLAGS="$altivec_flag"])
      CPPFLAGS="$altivec_save_CPPFLAGS"
      ;;
    *)
      ALTIVEC_EXTRA_CFLAGS="$altivec_flag"
      ;;
  esac
  AC_SUBST(ALTIVEC_EXTRA_CFLAGS)

  AC_MSG_CHECKING(whether we can compile AltiVec code)

  can_use_altivec=no
  if test -z "$ALTIVEC_EXTRA_CFLAGS"; then
    AC_MSG_RESULT(no)
    AC_MSG_WARN([The compiler does not support the AltiVec command set.])
  else
    case "$target_or_host" in
      *-*-darwin*)
        can_use_altivec=yes
        AC_DEFINE(HAVE_ALTIVEC_SYSCTL, 1,
         [Define to 1 if the altivec runtime test should use a sysctl.])
        ;;
      *)
        AC_COMPILE_IFELSE([AC_LANG_PROGRAM(,[asm ("vand %v0, %v0, %v0");])],
          can_use_altivec=yes, can_use_altivec=no)
        ;;
    esac
    AC_MSG_RESULT($can_use_altivec)

    if test "x$can_use_altivec" = "xyes"; then
      AC_DEFINE(USE_ALTIVEC, 1, [Define to 1 if AltiVec support is available.])
    else
      enable_altivec=no
      AC_MSG_WARN([The assembler does not support the AltiVec command set.])
    fi
  fi

  enable_altivec="$can_use_altivec"
fi


dnl ===========================================================================

AC_CHECK_HEADERS(dl.h)

AC_SEARCH_LIBS([dlopen], [dl])
AC_SEARCH_LIBS([rint], [m])

AC_REPLACE_FUNCS(gettimeofday)
AC_CHECK_FUNCS(rint)

#BABL_PATH_SEPARATOR must be defined as a character.
#BABL_DIR_SEPARATOR must be defined as a string.
AC_DEFINE_UNQUOTED(BABL_PATH_SEPARATOR, '$PATH_SEP', [separator between paths in BABL_PATH])
AC_DEFINE_UNQUOTED(BABL_DIR_SEPARATOR, "$DIR_SEP", [separator between directories in BABL_PATH])

AC_DEFINE_UNQUOTED(BABL_LIBRARY, "$PACKAGE_NAME-$BABL_API_VERSION", [name of BABL library])



dnl ===========================================================================

AC_CONFIG_FILES(
Makefile
babl.pc
babl/Makefile
babl/babl-version.h
babl/base/Makefile
docs/Makefile
docs/index-static.html
docs/graphics/Makefile
tests/Makefile
extensions/Makefile
INSTALL
)

AC_OUTPUT
