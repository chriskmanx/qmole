dnl
dnl $Header: /cvsroot/lesstif/lesstif/acinclude.m4,v 1.48 2005/11/01 08:14:56 dannybackx Exp $
dnl
dnl This file provides the following macros
dnl
dnl     AC_PATH_MOTIF_DIRECT
dnl     AC_PATH_MOTIF
dnl     AC_XT_VERSION6
dnl     AC_XT_VERSION5
dnl     LT_LIB_XTHREADS
dnl     AC_HAVE_EXCTAGS
dnl     LT_HAVE_GETHOSTNAME
dnl     LT_HAVE_MAN2HTML
dnl     LT_HTML2TXT
dnl



dnl
dnl Search for Motif by using explicit paths
dnl
AC_DEFUN([AC_PATH_MOTIF_DIRECT],
[test -z "$motif_direct_test_library" && motif_direct_test_library=Xm
test -z "$motif_direct_test_function" && motif_direct_test_function=XmCreatePushButton
test -z "$motif_direct_test_include" && motif_direct_test_include=Xm/Xm.h
  for ac_dir in               \
    /usr/include/Motif2.1     \
    /usr/Motif2.1/include     \
                              \
    /usr/include/Motif2.0     \
    /usr/Motif2.0/include     \
                              \
    /usr/include/Motif1.2     \
    /usr/Motif1.2/include     \
                              \
    /usr/motif/include        \
    /usr/lesstif/include      \
                              \
    /usr/X11R6/include        \
    /usr/X11R5/include        \
                              \
    /usr/include/X11R6        \
    /usr/include/X11R5        \
                              \
    /usr/local/X11R6/include  \
    /usr/local/X11R5/include  \
                              \
    /usr/local/include/X11R6  \
    /usr/local/include/X11R5  \
                              \
    /usr/X11/include          \
    /usr/include/X11          \
    /usr/local/X11/include    \
    /usr/local/include/X11    \
                              \
    /usr/X386/include         \
    /usr/x386/include         \
    /usr/XFree86/include/X11  \
                              \
    /usr/dt/include           \
    /usr/openwin/include      \
    /opt/openwin/include      \
                              \
    /usr/include              \
    /usr/local/include        \
    /usr/unsupported/include  \
    /usr/athena/include       \
    /usr/local/x11r5/include  \
    /usr/lpp/Xamples/include  \
    ; \
  do
    if test -r "$ac_dir/$motif_direct_test_include"; then
      no_motif= ac_motif_includes=$ac_dir
      break
    fi
  done

# Check for the libraries.
# See if we find them without any special options.
# Don't add to $LIBS permanently.
ac_save_LIBS="$LIBS"
LIBS="-l$motif_direct_test_library $LIBS"
# First see if replacing the include by lib works.
for ac_dir in `echo "$ac_motif_includes" | sed s/include/lib/` \
    /usr/lib/Motif2.1     \
    /usr/Motif2.1/lib     \
                          \
    /usr/lib/Motif2.0     \
    /usr/Motif2.0/lib     \
                          \
    /usr/lib/Motif1.2     \
    /usr/Motif1.2/lib     \
                          \
    /usr/motif/lib        \
    /usr/lesstif/lib      \
                          \
    /usr/X11R6/lib        \
    /usr/X11R5/lib        \
                          \
    /usr/lib/X11R6        \
    /usr/lib/X11R5        \
                          \
    /usr/local/X11R6/lib  \
    /usr/local/X11R5/lib  \
                          \
    /usr/local/lib/X11R6  \
    /usr/local/lib/X11R5  \
                          \
    /usr/X11/lib          \
    /usr/lib/X11          \
    /usr/local/X11/lib    \
                          \
    /usr/X386/lib         \
    /usr/x386/lib         \
    /usr/XFree86/lib/X11  \
                          \
    /usr/dt/lib           \
    /usr/openwin/lib      \
    /opt/openwin/lib      \
                          \
    /usr/lib              \
    /usr/local/lib        \
    /usr/unsupported/lib  \
    /usr/athena/lib       \
    /usr/local/x11r5/lib  \
    /usr/lpp/Xamples/lib  \
    ; \
do
  for ac_extension in "a" "so" "sl" "lib" ; do
    if test -r $ac_dir/lib${motif_direct_test_library}.$ac_extension; then
      no_motif= ac_motif_libraries=$ac_dir
      break 2
    fi
  done
done
LIBS=$ac_save_LIBS])



dnl
dnl Search for Motif
dnl
AC_DEFUN([AC_PATH_MOTIF],
[AC_REQUIRE_CPP()dnl

AC_ARG_WITH(motif-includes, [  --with-motif-includes=DIR     Motif include files are in DIR])
if test -z "$with_motif_includes"; then
  motif_includes=NONE
else
  motif_includes=$with_motif_includes
fi
AC_ARG_WITH(motif-libraries, [  --with-motif-libraries=DIR    Motif library files are in DIR])
if test -z "$with_motif_libraries"; then
  motif_libraries=NONE
else
  motif_libraries=$with_motif_libraries
fi

AC_MSG_CHECKING(for Motif)
AC_ARG_WITH(motif, [  --with-motif            enable Motif tests])
if test "x$with_motif" = xno; then
  no_motif=yes
else
  if test "x$motif_includes" != xNONE && test "x$motif_libraries" != xNONE; then
    no_motif=
  else
AC_CACHE_VAL(ac_cv_path_motif,
[# One or both of these vars are not set, and there is no cached value.
no_motif=yes

    #
    # Let's try a test link. If it works this will avoid putting the
    # default paths onto the compile and link lines.
    #
    ac_save_libs="$LIBS"
    ac_save_cflags="$CFLAGS"
    ac_save_cppflags="$CPPFLAGS"
    LIBS="-lXm $X_LIBS -lXt $X_PRE_LIBS -lX11 $X_EXTRA_LIBS $LIBS"
    CFLAGS="$X_CFLAGS $CFLAGS"
    CPPFLAGS="$X_CFLAGS $CPPFLAGS"

    AC_TRY_LINK(
        [#include <Xm/Label.h>],
	[Widget w; XmCreateLabel(w, "", NULL, 0);],
	[
	#
	# link passed, do nothing
	#
	no_motif="no"
	motif_includes=""
	motif_libraries=""
	],
	#
	# link failed, go search for it
	#
	AC_PATH_MOTIF_DIRECT
	) dnl AC_TRY_LINK

    LIBS="$ac_save_libs"
    CFLAGS="$ac_save_cflags"
    CPPFLAGS="$ac_save_cppflags"

if test "$no_motif" = yes; then
  ac_cv_path_motif="no_motif=yes"
else
  ac_cv_path_motif="no_motif= ac_motif_includes=$ac_motif_includes ac_motif_libraries=$ac_motif_libraries"
fi])dnl
  fi
  eval "$ac_cv_path_motif"
fi # with_motif != no

if test "$no_motif" = yes; then
  AC_MSG_RESULT(no)
else
  test "x$motif_includes" = xNONE && motif_includes=$ac_motif_includes
  test "x$motif_libraries" = xNONE && motif_libraries=$ac_motif_libraries
  ac_cv_path_motif="no_motif= ac_motif_includes=$motif_includes ac_motif_libraries=$motif_libraries"
  AC_MSG_RESULT([libraries $motif_libraries, headers $motif_includes])
fi
])



dnl
dnl Checks for Xt being version >= 6
dnl
AC_DEFUN([AC_XT_VERSION6],
 [AC_MSG_CHECKING([for Xt Revision Number 6])
  AC_TRY_LINK(
    [#include <X11/Intrinsic.h>],
    [
#if XtSpecificationRelease < 6
fail;
#endif
     ],
     XTversion="6",
     AC_MSG_RESULT(no)
     )
 ]
 )



dnl
dnl Checks for Xt being version >= 5
dnl
AC_DEFUN([AC_XT_VERSION5],
  [AC_MSG_CHECKING([for Xt Revision Number 5])
    AC_TRY_LINK(
     [#include <X11/Intrinsic.h>],
     [
#if XtSpecificationRelease < 5
fail;
#endif
     ],
     XTversion="5",
     AC_MSG_RESULT(no)
     )
   ]
)



dnl
dnl This should be the canonical approach to check for XTHREADS.
dnl (see "man XtToolkitThreadInitialize")
dnl
AC_DEFUN([LT_LIB_XTHREADS],
[AC_REQUIRE([AC_PATH_X])
AC_CACHE_CHECK(whether libXt was compiled with -DXTHREADS, lt_cv_xthreads,
[lt_save_CFLAGS="$CFLAGS"
lt_save_CPPFLAGS="$CPPFLAGS"
lt_save_LIBS="$LIBS"
LIBS="$X_LIBS -lXt $X_PRE_LIBS -lX11 $X_EXTRA_LIBS $LIBS"
CFLAGS="$X_CFLAGS $CFLAGS"
CPPFLAGS="$X_CFLAGS $CPPFLAGS"
AC_TRY_RUN(
[
#include <X11/Intrinsic.h>
int main() {
Boolean brc;
brc=XtToolkitThreadInitialize();
if (True==brc)
  exit(0);
else
  exit(1);
}
],
lt_cv_xthreads=yes,
lt_cv_xthreads=no,
lt_cv_xthreads=dunno)
])
if test $lt_cv_xthreads = dunno; then
  AC_MSG_WARN(Can't check for XTHREADS if cross-compiling. Assume XTHREADS)
  AC_DEFINE(XTHREADS)
fi
if test $lt_cv_xthreads = yes; then
  AC_DEFINE(XTHREADS)
fi
CFLAGS="$lt_save_CFLAGS"
CPPFLAGS="$lt_save_CPPFLAGS"
LIBS="$lt_save_LIBS"
])




dnl
dnl Checks whether Exuberant ctags is present
dnl (see http://darren.hiebert.com/ctags/index.html)
dnl
AC_DEFUN([AC_HAVE_EXCTAGS],
[
ac_have_exctags=no
AC_CHECK_PROG(ac_have_ctags, ctags, yes, no)
if test "$ac_have_ctags" = "yes"; then
  AC_MSG_CHECKING(if ctags is actually Exuberant ctags)
  if test -z "`ctags --version 2>/dev/null | grep Exuberant`" ; then
    ac_have_exctags=no
  else
    ac_have_exctags=yes
  fi
  AC_MSG_RESULT($ac_have_exctags)
fi 
AM_CONDITIONAL(Have_Exctags, test "$ac_have_exctags" = "yes")
])



dnl
dnl Extended check for gethostname()
dnl
AC_DEFUN([LT_HAVE_GETHOSTNAME],
[AC_REQUIRE([AC_PATH_X])
AC_CACHE_CHECK(whether gethostname() is available, lt_cv_gethostname,
[lt_save_CFLAGS="$CFLAGS"
lt_save_CPPFLAGS="$CPPFLAGS"
lt_save_LIBS="$LIBS"
LIBS="$X_EXTRA_LIBS"
CFLAGS="$X_CFLAGS $CFLAGS"
CPPFLAGS="$X_CFLAGS $CPPFLAGS"
AC_TRY_LINK([
#include <unistd.h>
],
[
#define namelen 256
char name[namelen];
int rc;
rc=gethostname(name, namelen);
],
lt_cv_gethostname=yes,
lt_cv_gethostname=no)
CFLAGS="$lt_save_CFLAGS"
CPPFLAGS="$lt_save_CPPFLAGS"
LIBS="$lt_save_LIBS"
])
if test $lt_cv_gethostname = yes; then
  AC_DEFINE(HAVE_GETHOSTNAME)
fi
])


dnl
dnl Checks whether man2html is present
dnl
dnl We've taken a quick look at the world and have found that man pages
dnl in traditional man page source are a good source. This source format
dnl can be translated into e.g. HTML easily.
dnl We use our own version of Debian's man2html.
dnl

AC_DEFUN([LT_HAVE_MAN2HTML],
[
AC_MSG_CHECKING(for suitable man2html)
AC_PATH_PROG(man2html_cmd, man2html)
if test -x "$man2html_cmd"
then
  if test -z "`$man2html_cmd </dev/null 2>&1 | grep \"LessTif's man2html\"`"
  then
     dnl found wrong one
     MAN2HTML="\$(top_builddir)/scripts/man2html"
     AC_MSG_RESULT(Use LessTif one)
  else
     dnl found right one
     MAN2HTML="$man2html_cmd"
     AC_MSG_RESULT($MAN2HTML)
  fi
else
  dnl found none
  MAN2HTML="\$(top_builddir)/scripts/man2html"
dnl  AC_MSG_RESULT($MAN2HTML)
fi
AC_SUBST(MAN2HTML)
AM_CONDITIONAL(LT_BUILD_MAN2HTML, test "$MAN2HTML" = "\$(top_builddir)/scripts/man2html")
])


dnl
dnl Look for a HTML to plain text converter.
dnl
dnl We check for the textmode web browsers 'lynx' and the less known 'links'.
dnl Finally we try a simple sed script, a very poor fallback indeed.
dnl
AC_DEFUN([LT_HTML2TXT],
[
AC_PATH_PROG(LYNX_CMD,lynx)
if test -x "$LYNX_CMD"
then
  HTML2TEXT="$LYNX_CMD -dump -force_html "
else
  AC_PATH_PROG(LINKS_CMD,links)
  if test -x "$LINKS_CMD"
  then
     HTML2TEXT="$LINKS_CMD -dump "
  else
     HTML2TEXT="sed -e 's/<[^<>]*>//g'"
  fi
fi
AC_SUBST(HTML2TEXT)
])

dnl
dnl Taken from /cvsroot/lesstif/lesstif/ac_find_xft.m4,v 1.6 2004/02/01 15:49:40 dannybackx Exp
dnl
dnl $XFree86: xc/lib/fontconfig/configure.in,v 1.7 2002/08/01 15:57:25 keithp Exp $
dnl
dnl Copyright © 2002 Keith Packard, member of The XFree86 Project, Inc.
dnl Manipulated into AC_FIND_XFT macro by Danny Backx (also © 2002).
dnl
dnl Permission to use, copy, modify, distribute, and sell this software and its
dnl documentation for any purpose is hereby granted without fee, provided that
dnl the above copyright notice appear in all copies and that both that
dnl copyright notice and this permission notice appear in supporting
dnl documentation, and that the name of Keith Packard not be used in
dnl advertising or publicity pertaining to distribution of the software without
dnl specific, written prior permission.  Keith Packard makes no
dnl representations about the suitability of this software for any purpose.  It
dnl is provided "as is" without express or implied warranty.
dnl
dnl KEITH PACKARD DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
dnl INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO
dnl EVENT SHALL KEITH PACKARD BE LIABLE FOR ANY SPECIAL, INDIRECT OR
dnl CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
dnl DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
dnl TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
dnl PERFORMANCE OF THIS SOFTWARE.
dnl

AC_DEFUN([AC_FIND_XFT],
[
AH_TEMPLATE([HAVE_FREETYPE], [We have the FreeType library])
AH_TEMPLATE([HAVE_FONTCONFIG], [We have the fontconfig library])
AH_TEMPLATE([HAVE_XRENDER], [We have the fontconfig library])
AH_TEMPLATE([FC_DEFAULT_FONTS], [We have the fontconfig library])
AH_TEMPLATE([X_FONT_DIR], [We have the fontconfig library])
AH_TEMPLATE([CONFDIR], [We have the fontconfig library])
AH_TEMPLATE([USE_XFT], [We have the fontconfig library])

AC_ARG_WITH(freetype_includes, [  --with-freetype-includes=DIR  Use FreeType includes in DIR], freetype_includes=$withval, freetype_includes=yes)
AC_ARG_WITH(freetype_lib,      [  --with-freetype-lib=DIR       Use FreeType library in DIR], freetype_lib=$withval, freetype_lib=yes)
AC_ARG_WITH(freetype_config,   [  --with-freetype-config=PROG   Use FreeType configuration program PROG], freetype_config=$withval, freetype_config=yes)
dnl AC_ARG_WITH(expat,             [  --with-expat=DIR              Use Expat in DIR], expat=$withval, expat=yes)
dnl AC_ARG_WITH(expat_includes,    [  --with-expat-includes=DIR     Use Expat includes in DIR], expat_includes=$withval, expat_includes=yes)
dnl AC_ARG_WITH(expat_lib,         [  --with-expat-lib=DIR          Use Expat library in DIR], expat_lib=$withval, expat_lib=yes)
AC_ARG_WITH(default_fonts,     [  --with-default-fonts=DIR      Use fonts from DIR when config is busted], defaultfonts="$withval", default_fonts=yes)
dnl AC_ARG_WITH(confdir,           [  --with-confdir=DIR            Use DIR to store configuration files (default /etc/fonts)], confdir="$withval", confdir=yes)
AC_ARG_WITH(fontconfig_includes, [  --with-fontconfig-includes=DIR  Use Fontconfig includes in DIR], fontconfig_includes=$withval, fontconfig_includes=yes)
AC_ARG_WITH(fontconfig_lib,      [  --with-fontconfig-lib=DIR       Use Fontconfig library in DIR], fontconfig_lib=$withval, fontconfig_lib=yes)
AC_ARG_WITH(fontconfig_config,   [  --with-fontconfig-config=PROG  Use Fontconfig configuration program PROG], fontconfig_config=$withval, fontconfig_config=yes)

# Using x libraries, set X font directory
case "$no_x" in
yes)
	;;
*)
	X_FONT_DIR="$x_libraries/X11/fonts"
	AC_DEFINE_UNQUOTED(X_FONT_DIR,$X_FONT_DIR)
	;;
esac
AC_SUBST(X_FONT_DIR)

#
# Check freetype configuration
#
case "$freetype_config" in
no)
	;;
yes)
	AC_CHECK_PROG(ft_config,freetype-config,freetype-config,no)
	;;
*)
	ft_config="$freetype_config"
	;;
esac

case "$freetype_includes" in
no)
	FREETYPE_CFLAGS=""
	;;
yes)
	case "$ft_config" in
	no)
		FREETYPE_CFLAGS=""
		;;
	*)
		FREETYPE_CFLAGS="`$ft_config --cflags`"
		;;
	esac
	;;
*)
	FREETYPE_CFLAGS="-I$freetype_includes"
	;;
esac

case "$freetype_lib" in
no)
	freetype_lib=""
	;;
yes)
	case "$ft_config" in
	no)
		freetype_lib=""
		;;
	*)
		freetype_lib="`$ft_config --libs`"
		;;
	esac
	;;
*)
	freetype_lib="-L$freetype_lib -lfreetype"
	;;
esac

saved_LIBS="$LIBS"
LIBS="$LIBS $freetype_lib"
saved_CPPFLAGS="$CPPFLAGS"
CPPFLAGS="$CPPFLAGS $FREETYPE_CFLAGS"
AC_CHECK_HEADERS(freetype/freetype.h,,, [#include <ft2build.h>])

LT_HAVE_FREETYPE="no"
case "$ac_cv_header_freetype_freetype_h" in
no)
	CPPFLAGS="$saved_CPPFLAGS"
	LIBS="$saved_LIBS"
	;;
yes)
	AC_CHECK_FUNCS(FT_Init_FreeType)
	case "$ac_cv_func_FT_Init_FreeType" in
	no)
		CPPFLAGS="$saved_CPPFLAGS"
		LIBS="$saved_LIBS"
		;;
	yes)
		LT_HAVE_FREETYPE="yes"
		AC_DEFINE(HAVE_FREETYPE, 1, Means we have discovered the FreeType library)
		AC_SUBST(FREETYPE_CFLAGS)
		;;
	esac
	;;
esac

case "$default_fonts" in
yes)
	FC_DEFAULT_FONTS="/usr/share/fonts"
	AC_DEFINE_UNQUOTED(FC_DEFAULT_FONTS, "/usr/share/fonts")
	;;
*)
	FC_DEFAULT_FONTS="$default_fonts"
	AC_DEFINE_UNQUOTED(FC_DEFAULT_FONTS, "$default_fonts")
	;;
esac

AC_SUBST(FC_DEFAULT_FONTS)

#
# Set CONFDIR and FONTCONFIG_PATH
#

case "$confdir" in
no|yes)
	confdir=/etc/fonts
	;;
*)
	;;
esac
AC_SUBST(confdir)
CONFDIR="${confdir}"
AC_DEFINE_UNQUOTED(CONFDIR, "$CONFDIR")
AC_SUBST(CONFDIR)

#
# Check X configuration
#
LT_HAVE_XRENDER="no"
case "$have_x" in
yes)
	XRENDER_CFLAGS="-I$x_includes"
	XRENDER_LIBS="-L$x_libraries -lXft -lXrender"

	saved_LIBS="$LIBS"
	LIBS="$LIBS $XRENDER_LIBS"
	saved_CPPFLAGS="$CPPFLAGS"
	CPPFLAGS="$CPPFLAGS $XRENDER_CFLAGS"
	AC_CHECK_HEADERS(X11/extensions/Xrender.h)

	case "$ac_cv_header_X11_extensions_Xrender_h" in
	no)
		CPPFLAGS="$saved_CPPFLAGS"
		LIBS="$saved_LIBS"
		;;
	yes)
		AC_CHECK_FUNCS(XRenderParseColor)
		case "$ac_cv_func_XRenderParseColor" in
		no)
			CPPFLAGS="$saved_CPPFLAGS"
			LIBS="$saved_LIBS"
			;;
		yes)
			LT_HAVE_XRENDER="yes"
			AC_DEFINE(HAVE_XRENDER, 1, Means we have discovered the Xrender library)
			AC_SUBST(XRENDER_CFLAGS)
			AC_SUBST(XRENDER_LIBS)
			;;
		esac
		;;
	esac

	;;
esac

#
# Check fontconfig configuration
#
case "$fontconfig_config" in
no)
	;;
yes)
	AC_CHECK_PROG(fc_config,fontconfig-config,fontconfig-config,no)
	;;
*)
	fc_config="$fontconfig_config"
	;;
esac

case "$fontconfig_includes" in
no)
	FONTCONFIG_CFLAGS=""
	;;
yes)
	case "$fc_config" in
	no)
		FONTCONFIG_CFLAGS=""
		;;
	*)
		FONTCONFIG_CFLAGS="`$fc_config --cflags`"
		;;
	esac
	;;
*)
	FONTCONFIG_CFLAGS="-I$fontconfig_includes"
	;;
esac

case "$fontconfig_lib" in
no)
	fontconfig_lib=""
	;;
yes)
	case "$fc_config" in
	no)
		fontconfig_lib=""
		;;
	*)
		FONTCONFIG_LIBS="`$fc_config --libs`"
		;;
	esac
	;;
*)
	FONTCONFIG_LIBS="-L$fontconfig_lib -lfontconfig"
	;;
esac

saved_LIBS="$LIBS"
LIBS="$LIBS $FONTCONFIG_LIBS"
saved_CPPFLAGS="$CPPFLAGS"
CPPFLAGS="$CPPFLAGS $FONTCONFIG_CFLAGS"
AC_CHECK_HEADERS(fontconfig/fontconfig.h)

case "$ac_cv_header_fontconfig_fontconfig_h" in
no)
	CPPFLAGS="$saved_CPPFLAGS"
	LIBS="$saved_LIBS"
	;;
yes)
	AC_CHECK_FUNCS(FcInit)
	case "$ac_cv_func_FcInit" in
	no)
		CPPFLAGS="$saved_CPPFLAGS"
		LIBS="$saved_LIBS"
		;;
	yes)
		AC_DEFINE(HAVE_FONTCONFIG, 1, This is defined if we find the FontConfig library)
		AC_SUBST(FONTCONFIG_CFLAGS)
		AC_SUBST(FONTCONFIG_LIBS)
		;;
	esac
	;;
esac

dnl
dnl Should this be conditionally defined ?
dnl
if test "$LT_HAVE_XRENDER" = "yes" -a "$LT_HAVE_FREETYPE" = "yes"
then
	AC_DEFINE(USE_XFT, 1, This means we will be using the Xft library)
fi
])


dnl
dnl
dnl End of special macros.
dnl
dnl
