dnl
dnl $Header: /cvsroot/lesstif/lesstif/scripts/autoconf/ac_find_motif.m4,v 1.8 2004/02/01 15:55:07 dannybackx Exp $
dnl
dnl AC_FIND_MOTIF : find OSF/Motif or LessTif, and provide variables
dnl	to easily use them in a Makefile.
dnl
dnl Adapted from a macro by Andreas Zeller.
dnl
dnl The variables provided are :
dnl	link_motif		(e.g. -L/usr/lesstif/lib -lXm)
dnl	include_motif		(e.g. -I/usr/lesstif/lib)
dnl	motif_libraries		(e.g. /usr/lesstif/lib)
dnl	motif_includes		(e.g. /usr/lesstif/include)
dnl
dnl The link_motif and include_motif variables should be fit to put on
dnl your application's link line in your Makefile.
dnl
dnl Oleo CVS Id: motif.m4,v 1.9 1999/04/09 11:46:49 danny
dnl LessTif CVS $Id: ac_find_motif.m4,v 1.8 2004/02/01 15:55:07 dannybackx Exp $
dnl
AC_DEFUN([AC_FIND_MOTIF],
[

AC_REQUIRE([AC_PATH_XTRA])
AC_REQUIRE([AC_FIND_LIBXP])

motif_includes=
motif_libraries=

dnl AC_ARG_WITH(motif,
dnl [  --without-motif         do not use Motif widgets])
dnl Treat --without-motif like
dnl --without-motif-includes --without-motif-libraries.
dnl if test "$with_motif" = "no"
dnl then
dnl   motif_includes=none
dnl   motif_libraries=none
dnl fi

AC_ARG_WITH(motif-includes,
[  --with-motif-includes=DIR    Motif include files are in DIR],
motif_includes="$withval")

AC_ARG_WITH(motif-libraries,
[  --with-motif-libraries=DIR   Motif libraries are in DIR],
motif_libraries="$withval")

AC_MSG_CHECKING(for Motif)

#
#
# Search the include files.
#
if test "$motif_includes" = ""; then
AC_CACHE_VAL(ac_cv_motif_includes,
[
ac_motif_save_LIBS="$LIBS"
ac_motif_save_INCLUDES="$INCLUDES"
ac_motif_save_CPPFLAGS="$CPPFLAGS"
ac_motif_save_LDFLAGS="$LDFLAGS"
#
LIBS="$X_PRE_LIBS -lXm -lXt -lX11 $X_EXTRA_LIBS $LIBS"
INCLUDES="$X_CFLAGS $INCLUDES"
CPPFLAGS="$X_CFLAGS $CPPFLAGS"
LDFLAGS="$X_LIBS $LDFLAGS"
#
ac_cv_motif_includes="none"
AC_TRY_COMPILE([#include <Xm/Xm.h>],[int a;],
[
# Xm/Xm.h is in the standard search path.
ac_cv_motif_includes=
],
[
# Xm/Xm.h is not in the standard search path.
# Locate it and put its directory in `motif_includes'
#
# /usr/include/Motif* are used on HP-UX (Motif).
# /usr/include/X11* are used on HP-UX (X and Athena).
# /usr/dt is used on Solaris (Motif).
# /usr/openwin is used on Solaris (X and Athena).
# Other directories are just guesses.
for dir in "$x_includes" "${prefix}/include" /usr/include /usr/local/include \
           /usr/include/Motif2.1 /usr/include/Motif2.0 /usr/include/Motif1.2  \
           /usr/include/X11 /usr/include/X11R6 /usr/include/X11R5 \
           /usr/X11/include /usr/X11R6/include /usr/X11R5/include \
           /usr/dt/include /usr/openwin/include \
           /usr/dt/*/include /opt/*/include /usr/include/Motif* \
           "${prefix}"/*/include /usr/*/include /usr/local/*/include \
           "${prefix}"/include/* /usr/include/* /usr/local/include/*; do
if test -f "$dir/Xm/Xm.h"; then
ac_cv_motif_includes="$dir"
break
fi
done
])
#
LIBS="$ac_motif_save_LIBS"
INCLUDES="$ac_motif_save_INCLUDES"
CPPFLAGS="$ac_motif_save_CPPFLAGS"
LDFLAGS="$ac_motif_save_LDFLAGS"
])
motif_includes="$ac_cv_motif_includes"
fi
#
#
# Now for the libraries.
#
if test "$motif_libraries" = ""; then
AC_CACHE_VAL(ac_cv_motif_libraries,
[
ac_motif_save_LIBS="$LIBS"
ac_motif_save_INCLUDES="$INCLUDES"
ac_motif_save_CPPFLAGS="$CPPFLAGS"
ac_motif_save_LDFLAGS="$LDFLAGS"
#
LIBS="$X_PRE_LIBS -lXm -lXt -lX11 $X_EXTRA_LIBS $LIBS"
INCLUDES="$X_CFLAGS $INCLUDES"
CPPFLAGS="$X_CFLAGS $CPPFLAGS"
LDFLAGS="$X_LIBS $LDFLAGS"
#
ac_cv_motif_libraries="none"
AC_TRY_LINK([#include <Xm/Xm.h>],[XtToolkitInitialize();],
[
# libXm.a is in the standard search path.
ac_cv_motif_libraries=
],
[
# libXm.a is not in the standard search path.
# Locate it and put its directory in `motif_libraries'
#
# /usr/lib/Motif* are used on HP-UX (Motif).
# /usr/lib/X11* are used on HP-UX (X and Athena).
# /usr/dt is used on Solaris (Motif).
# /usr/lesstif is used on Linux (Lesstif).
# /usr/openwin is used on Solaris (X and Athena).
# Other directories are just guesses.
for dir in "$x_libraries" "${prefix}/lib" /usr/lib /usr/local/lib \
           /usr/lib/Motif2.1 /usr/lib/Motif2.0 /usr/lib/Motif1.2 \
           /usr/lib/X11 /usr/lib/X11R6 /usr/lib/X11R5 \
           /usr/X11/lib /usr/X11R6/lib /usr/X11R5/lib \
           /usr/dt/lib /usr/openwin/lib \
           /usr/dt/*/lib /opt/*/lib /usr/lib/Motif* \
           /usr/lesstif*/lib /usr/lib/Lesstif* \
           "${prefix}"/*/lib /usr/*/lib /usr/local/*/lib \
           "${prefix}"/lib/* /usr/lib/* /usr/local/lib/*; do
if test -d "$dir" && test "`ls $dir/libXm.* 2> /dev/null`" != ""; then
ac_cv_motif_libraries="$dir"
break
fi
done
])
#
LIBS="$ac_motif_save_LIBS"
INCLUDES="$ac_motif_save_INCLUDES"
CPPFLAGS="$ac_motif_save_CPPFLAGS"
LDFLAGS="$ac_motif_save_LDFLAGS"
])
#
motif_libraries="$ac_cv_motif_libraries"
fi
#
# Provide an easier way to link
#
if test "$motif_includes" = "none" -o "$motif_libraries" = "none"; then
        with_motif="no"
else
        with_motif="yes"
fi

AC_FIND_LIBXP
if test "$LT_HAVE_XP" = "yes"; then
	XPLIB="-lXp -lXext"
else
	XPLIB=""
fi

if test "$with_motif" != "no"; then
        if test "$motif_libraries" = ""; then
                link_motif="-lXm $XPLIB"
                MOTIF_LIBS="-lXm $XPLIB"
        else
                link_motif="-L$motif_libraries -lXm $XPLIB"
                MOTIF_LIBS="-L$motif_libraries -lXm $XPLIB"
        fi
        if test "$motif_includes" != ""; then
                include_motif="-I$motif_includes"
                MOTIF_CFLAGS="-I$motif_includes"
        fi
	AC_DEFINE(HAVE_MOTIF)
else
        with_motif="no"
fi
#
AC_SUBST(link_motif)
AC_SUBST(include_motif)
AC_SUBST(MOTIF_CFLAGS)
AC_SUBST(MOTIF_LIBS)
#
#
#
motif_libraries_result="$motif_libraries"
motif_includes_result="$motif_includes"
test "$motif_libraries_result" = "" && motif_libraries_result="in default path"
test "$motif_includes_result" = "" && motif_includes_result="in default path"
test "$motif_libraries_result" = "none" && motif_libraries_result="(none)"
test "$motif_includes_result" = "none" && motif_includes_result="(none)"
AC_MSG_RESULT(
  [libraries $motif_libraries_result, headers $motif_includes_result])
])dnl

dnl
dnl Check for libXp
dnl In fact this check ensures that
dnl  - <X11/extensions/Print.h> and
dnl  - both libXp libXext
dnl are in place
dnl Note that a simpler check only for the libraries would not
dnl be sufficient perhaps.
dnl If the test succeeds it defines Have_Libxp within our
dnl Makefiles. Perhaps one should immediately add those libs
dnl to link commands which include libXm version2.1?!
dnl
AC_DEFUN([AC_FIND_LIBXP],
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
int main() {
Display *display=NULL;
short   major_version, minor_version;
Status rc;
rc=XpQueryVersion(display, &major_version, &minor_version);
exit(0);
}
],
lt_cv_libxp=yes,
lt_cv_libxp=no)
])
if test "$lt_cv_libxp" = "yes"; then
  AC_DEFINE(HAVE_LIB_XP)
  LT_HAVE_XP="yes"
else
  LT_HAVE_XP="no"
fi
AM_CONDITIONAL(Have_Libxp, test "$lt_cv_libxp" = "yes")
AC_SUBST(LT_HAVE_XP)
CFLAGS="$lt_save_CFLAGS"
CPPFLAGS="$lt_save_CPPFLAGS"
LIBS="$lt_save_LIBS"
])

