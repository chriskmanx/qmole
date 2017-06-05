dnl
dnl Check for libXp
dnl In fact this check ensures that
dnl  - <X11/extensions/Print.h> and
dnl  - both libXp and libXext
dnl are in place
dnl Perhaps AC_CHECK_LIB() could be used as well, but
dnl requires the same amount of work to get all linker
dnl flags and additional libraries specified.
dnl If the test succeeds 'Have_Libxp' will be defined within our 
dnl Makefiles. Perhaps one should immediately add those libs
dnl to link commands which include libXm version2.1?!
dnl
AC_DEFUN([LT_HAVE_LIBXP],
[AC_REQUIRE([AC_PATH_X])
AC_CACHE_CHECK(whether libXp is available, lt_cv_libxp,
[lt_save_CFLAGS="$CFLAGS"
lt_save_CPPFLAGS="$CPPFLAGS"
lt_save_LIBS="$LIBS"
LIBS="$X_LIBS -lXp -lXext -lXt $X_PRE_LIBS -lX11 $X_EXTRA_LIBS $LIBS"
CFLAGS="$X_CFLAGS $CFLAGS"
CPPFLAGS="$X_CFLAGS $CPPFLAGS"
AC_TRY_LINK([
#include <X11/Intrinsic.h>
#include <X11/extensions/Print.h>
],[
Display *display=NULL;
short   major_version, minor_version;
Status rc;
rc=XpQueryVersion(display, &major_version, &minor_version);
],
lt_cv_libxp=yes,
lt_cv_libxp=no)
])
if test $lt_cv_libxp = yes; then
  AC_DEFINE(HAVE_LIB_XP)
  LT_HAVELIBXP=1
else
  LT_HAVELIBXP=0 
fi
AM_CONDITIONAL(Have_Libxp, test "$lt_cv_libxp" = "yes")
AC_SUBST(LT_HAVELIBXP)
CFLAGS="$lt_save_CFLAGS"
CPPFLAGS="$lt_save_CPPFLAGS"
LIBS="$lt_save_LIBS"
])
