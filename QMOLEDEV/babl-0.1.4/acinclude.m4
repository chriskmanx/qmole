dnl BABL_DETECT_CFLAGS(RESULT, FLAGSET)
dnl Detect if the compiler supports a set of flags

AC_DEFUN([BABL_DETECT_CFLAGS],
[
  $1=
  for flag in $2; do
    if test -z "[$]$1"; then
      $1_save_CFLAGS="$CFLAGS"
      CFLAGS="$CFLAGS $flag"
      AC_MSG_CHECKING([whether [$]CC understands [$]flag])
      AC_TRY_COMPILE([], [], [$1_works=yes], [$1_works=no])
      AC_MSG_RESULT([$]$1_works)
      CFLAGS="[$]$1_save_CFLAGS"
      if test "x[$]$1_works" = "xyes"; then
        $1="$flag"
      fi
    fi
  done
])


# BABL_VARIADIC_MACROS
# --------------------
# check for flavours of variadic macros
AC_DEFUN([BABL_VARIADIC_MACROS],
[AC_BEFORE([$0], [AC_PROG_CXX])
AC_REQUIRE([AC_PROG_CPP])
AC_CACHE_CHECK([for GNUC variadic macros],
    [babl_cv_prog_gnuc_variadic_macros],
    [# gcc-2.95.x supports both gnu style and ISO varargs, but if -ansi
    # is passed ISO vararg support is turned off, and there is no work
    # around to turn it on, so we unconditionally turn it off.
    { echo '#if __GNUC__ == 2 && __GNUC_MINOR__ == 95'
      echo ' yes '
      echo '#endif'; } > conftest.c
    if ${CPP} conftest.c | grep yes > /dev/null; then
        babl_cv_prog_c_variadic_macros=no
        babl_cv_prog_cxx_variadic_macros=no
    fi
    AC_TRY_COMPILE([],[int a(int p1, int p2, int p3);
#define call_a(params...) a(1,params)
call_a(2,3);
    ],
        babl_cv_prog_gnuc_variadic_macros=yes,
        babl_cv_prog_gnuc_variadic_macros=no)
])
if test x$babl_cv_prog_gnuc_variadic_macros = xyes; then
  AC_DEFINE(BABL_GNUC_VARIADIC_MACROS, 1, [Define if the C pre-processor supports GNU style variadic macros])
fi

AC_CACHE_CHECK([for ISO C99 variadic macros in C],
    [babl_cv_prog_c_variadic_macros],
    [AC_TRY_COMPILE([],[int a(int p1, int p2, int p3);
#define call_a(...) a(1,__VA_ARGS__)
call_a(2,3);
        ],
        babl_cv_prog_c_variadic_macros=yes,
        babl_cv_prog_c_variadic_macros=no)
])
if test x$babl_cv_prog_c_variadic_macros = xyes ; then
  AC_DEFINE(BABL_ISO_VARIADIC_MACROS, 1, [Define if the C pre-processor supports variadic macros])
fi

AC_PROVIDE_IFELSE([AC_PROG_CXX],
    [AC_CACHE_CHECK([for ISO C99 variadic macros in C++],
        [babl_cv_prog_cxx_variadic_macros],
        [AC_LANG_PUSH(C++)
        AC_TRY_COMPILE([],[int a(int p1, int p2, int p3);
#define call_a(...) a(1,__VA_ARGS__)
call_a(2,3);
        ],
        babl_cv_prog_cxx_variadic_macros=yes,
        babl_cv_prog_cxx_variadic_macros=no)
        AC_LANG_POP])],
    [# No C++ compiler
    babl_cv_prog_cxx_variadic_macros=no])
if test x$babl_cv_prog_cxx_variadic_macros = xyes ; then
  AC_DEFINE(BABL_ISO_CXX_VARIADIC_MACROS, 1, [Define if the C++ pre-processor supports variadic macros])
fi
])

