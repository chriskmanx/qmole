dnl# $Id: configure.ac 50969 2010-08-10 18:08:44Z kwo $

PATH=$PATH:/usr/bin:/bin:/usr/local/bin:/usr/ccs/bin:/usr/contrib/bin:/usr/ucb/bin
export PATH

test "x$LIBTOOLIZE" = "x" && LIBTOOLIZE=libtoolize
test "x$ACLOCAL" = "x" && ACLOCAL=aclocal
test "x$AUTOCONF" = "x" && AUTOCONF=autoconf
test "x$AUTOHEADER" = "x" && AUTOHEADER=autoheader
test "x$AUTOMAKE" = "x" && AUTOMAKE=automake

AC_SUBST(LIBTOOLIZE)
AC_SUBST(ACLOCAL)
AC_SUBST(AUTOCONF)
AC_SUBST(AUTOHEADER)
AC_SUBST(AUTOMAKE)

SAVE_LIBTOOLIZE="$LIBTOOLIZE"
SAVE_ACLOCAL="$ACLOCAL"
SAVE_AUTOCONF="$AUTOCONF"
SAVE_AUTOHEADER="$AUTOHEADER"
SAVE_AUTOMAKE="$AUTOMAKE"

AC_INIT(src/feature.h)
AM_INIT_AUTOMAKE(Eterm, 0.9.6)

dnl# Set some basic variables
DATE="`date '+%d %B %Y'`"
AC_SUBST(DATE)
BUILD_DATE="`date`"
AC_DEFINE_UNQUOTED(BUILD_DATE, "$BUILD_DATE", [The date of the build.])
AUTHORS="Michael Jennings (mej@eterm.org) "
AC_SUBST(AUTHORS)
AC_DEFINE_UNQUOTED(AUTHORS, "$AUTHORS", [Authors])

LIBTOOLIZE="${SAVE_LIBTOOLIZE:-libtoolize}"
ACLOCAL="${SAVE_ACLOCAL:-aclocal}"
AUTOCONF="${SAVE_AUTOCONF:-autoconf}"
AUTOHEADER="${SAVE_AUTOHEADER:-autoheader}"
AUTOMAKE="${SAVE_AUTOMAKE:-automake}"

AC_MSG_CHECKING(for preferred libtoolize)
AC_MSG_RESULT($LIBTOOLIZE)
AC_MSG_CHECKING(for preferred aclocal)
AC_MSG_RESULT($ACLOCAL)
AC_MSG_CHECKING(for preferred autoconf)
AC_MSG_RESULT($AUTOCONF)
AC_MSG_CHECKING(for preferred autoheader)
AC_MSG_RESULT($AUTOHEADER)
AC_MSG_CHECKING(for preferred automake)
AC_MSG_RESULT($AUTOMAKE)

dnl# Find RCS Ident info from source files
AC_DEFINE_UNQUOTED(ACTIONS_IDENT, "`grep '$''Id:' $srcdir/src/actions.c | sed 's/^.*\$''Id: //;s/ \$.*$//;s/.v / /;s/ Exp//'`", [actions.c RCS ID])
AC_DEFINE_UNQUOTED(BUTTONS_IDENT, "`grep '$''Id:' $srcdir/src/buttons.c | sed 's/^.*\$''Id: //;s/ \$.*$//;s/.v / /;s/ Exp//'`", [buttons.c RCS ID])
AC_DEFINE_UNQUOTED(COMMAND_IDENT, "`grep '$''Id:' $srcdir/src/command.c | sed 's/^.*\$''Id: //;s/ \$.*$//;s/.v / /;s/ Exp//'`", [command.c RCS ID])
AC_DEFINE_UNQUOTED(DRAW_IDENT, "`grep '$''Id:' $srcdir/src/draw.c | sed 's/^.*\$''Id: //;s/ \$.*$//;s/.v / /;s/ Exp//'`", [draw.c RCS ID])
AC_DEFINE_UNQUOTED(E_IDENT, "`grep '$''Id:' $srcdir/src/e.c | sed 's/^.*\$''Id: //;s/ \$.*$//;s/.v / /;s/ Exp//'`", [e.c RCS ID])
AC_DEFINE_UNQUOTED(EVENTS_IDENT, "`grep '$''Id:' $srcdir/src/events.c | sed 's/^.*\$''Id: //;s/ \$.*$//;s/.v / /;s/ Exp//'`", [events.c RCS ID])
AC_DEFINE_UNQUOTED(FONT_IDENT, "`grep '$''Id:' $srcdir/src/font.c | sed 's/^.*\$''Id: //;s/ \$.*$//;s/.v / /;s/ Exp//'`", [font.c RCS ID])
AC_DEFINE_UNQUOTED(GRKELOT_IDENT, "`grep '$''Id:' $srcdir/src/grkelot.c | sed 's/^.*\$''Id: //;s/ \$.*$//;s/.v / /;s/ Exp//'`", [grkelot.c RCS ID])
AC_DEFINE_UNQUOTED(MAIN_IDENT, "`grep '$''Id:' $srcdir/src/main.c | sed 's/^.*\$''Id: //;s/ \$.*$//;s/.v / /;s/ Exp//'`", [main.c RCS ID])
AC_DEFINE_UNQUOTED(MENUS_IDENT, "`grep '$''Id:' $srcdir/src/menus.c | sed 's/^.*\$''Id: //;s/ \$.*$//;s/.v / /;s/ Exp//'`", [menus.c RCS ID])
AC_DEFINE_UNQUOTED(MISC_IDENT, "`grep '$''Id:' $srcdir/src/misc.c | sed 's/^.*\$''Id: //;s/ \$.*$//;s/.v / /;s/ Exp//'`", [misc.c RCS ID])
AC_DEFINE_UNQUOTED(OPTIONS_IDENT, "`grep '$''Id:' $srcdir/src/options.c | sed 's/^.*\$''Id: //;s/ \$.*$//;s/.v / /;s/ Exp//'`", [options.c RCS ID])
AC_DEFINE_UNQUOTED(PIXMAP_IDENT, "`grep '$''Id:' $srcdir/src/pixmap.c | sed 's/^.*\$''Id: //;s/ \$.*$//;s/.v / /;s/ Exp//'`", [pixmap.c RCS ID])
AC_DEFINE_UNQUOTED(SCREEN_IDENT, "`grep '$''Id:' $srcdir/src/screen.c | sed 's/^.*\$''Id: //;s/ \$.*$//;s/.v / /;s/ Exp//'`", [screen.c RCS ID])
AC_DEFINE_UNQUOTED(SCROLLBAR_IDENT, "`grep '$''Id:' $srcdir/src/scrollbar.c | sed 's/^.*\$''Id: //;s/ \$.*$//;s/.v / /;s/ Exp//'`", [scrollbar.c RCS ID])
AC_DEFINE_UNQUOTED(STARTUP_IDENT, "`grep '$''Id:' $srcdir/src/startup.c | sed 's/^.*\$''Id: //;s/ \$.*$//;s/.v / /;s/ Exp//'`", [startup.c RCS ID])
AC_DEFINE_UNQUOTED(SYSTEM_IDENT, "`grep '$''Id:' $srcdir/src/system.c | sed 's/^.*\$''Id: //;s/ \$.*$//;s/.v / /;s/ Exp//'`", [system.c RCS ID])
AC_DEFINE_UNQUOTED(TERM_IDENT, "`grep '$''Id:' $srcdir/src/term.c | sed 's/^.*\$''Id: //;s/ \$.*$//;s/.v / /;s/ Exp//'`", [term.c RCS ID])
AC_DEFINE_UNQUOTED(TIMER_IDENT, "`grep '$''Id:' $srcdir/src/timer.c | sed 's/^.*\$''Id: //;s/ \$.*$//;s/.v / /;s/ Exp//'`", [timer.c RCS ID])
AC_DEFINE_UNQUOTED(UTMP_IDENT, "`grep '$''Id:' $srcdir/src/utmp.c | sed 's/^.*\$''Id: //;s/ \$.*$//;s/.v / /;s/ Exp//'`", [utmp.c RCS ID])
AC_DEFINE_UNQUOTED(WINDOWS_IDENT, "`grep '$''Id:' $srcdir/src/windows.c | sed 's/^.*\$''Id: //;s/ \$.*$//;s/.v / /;s/ Exp//'`", [windows.c RCS ID])

dnl# Check for build/host/target system types
AC_CANONICAL_HOST

dnl# Supply default CFLAGS, if not specified by `CFLAGS=flags ./configure'
if test ! -z "${CFLAGS}" -o ! -z "${CCOPTS}"; then
  CFLAGS_GIVEN=1
else
  CFLAGS_GIVEN=0
fi

if test -z "${CFLAGS}"; then
    if test -z "${CCOPTS}"; then
	CCOPTS="-O"
    fi
    CFLAGS="$CCOPTS"
fi

AC_ARG_WITH(cc, [  --with-cc=compiler      force Eterm to build with a particular compiler (like pgcc, egcs, etc.)], [
            CC=$withval, 
  if test -z "$CC"; then
    CC=check
  fi
])

if test "$CC" = "check"; then
  dnl# Check for Pentium compiler
  AC_CHECK_PROG(PGCC, pgcc, pgcc)

  if test ! -z "$PGCC"; then
    CC=$PGCC
  else
    unset CC
  fi
fi

AC_PROG_CC
AC_PROG_CPP
AM_PROG_AS

dnl# These must be run after AC_PROG_CC but before any other macros that use
dnl# the C compiler
AC_AIX
AC_ISC_POSIX
AC_MINIX

dnl# At least make the attempt to support CygWin32
AC_CYGWIN
AC_ARG_PROGRAM

AM_PROG_LIBTOOL
AC_PROG_LIBTOOL

dnl# Where are we?
AC_MSG_CHECKING(for distribution root)
DIST_ROOT=`pwd`
AC_SUBST(DIST_ROOT)
AC_MSG_RESULT($DIST_ROOT)

AC_GCC_TRADITIONAL

dnl # If using gcc, use -O2.  If -g works with it, use that too
if test "${CFLAGS_GIVEN}" -ne 1; then
    if test "$GCC" = "yes"; then
	if test "$ac_cv_prog_cc_g" = "yes"; then
	    CCOPTS='-g -O2'
	else
	    CCOPTS='-O2'
	fi
	CFLAGS="$CCOPTS"
    fi
fi

AC_PROG_INSTALL

CCASFLAGS="$CCASFLAGS -I.. -I."
CCAS=$CC
AC_SUBST(CCASFLAGS)
AC_SUBST(CCAS)

dnl# Check the sanity of what we've done so far
AM_SANITY_CHECK

dnl# Most people don't want the developer-only clutter
AM_MAINTAINER_MODE

dnl# If it's there, what the hell?
AM_WITH_DMALLOC

dnl# Look for needed programs
AC_CHECK_PROG(SED, sed, sed, false)
AC_CHECK_PROG(RM, rm, rm, true)
AC_CHECK_PROG(CP, cp, cp, false)
AC_CHECK_PROG(CHMOD, chmod, chmod, true)
AC_CHECK_PROG(TAR, tar, tar, tar)
AC_CHECK_PROG(MKDIR, mkdir, mkdir, false)
AC_CHECK_PROG(CTAGS, ctags, ctags, true)
AC_CHECK_PROG(AR, ar, ar, false)
AC_CHECK_PROG(MV, mv, mv, true)
AC_CHECK_PROG(TIC, tic, tic, true)
AC_LN_S
AC_PATH_PROG(PERL, perl, perl)
AC_PROG_AWK
export PERL AWK

AC_C_CONST
AC_C_INLINE
AC_C_BIGENDIAN

AC_PATH_XTRA

if test ! -z "$X_CFLAGS"; then
  if test -z "$CPPFLAGS"; then
    CPPFLAGS="$X_CFLAGS"
  else
    CPPFLAGS="$CPPFLAGS $X_CFLAGS"
  fi
fi
if test ! -z "$X_LIBS"; then
  if test -z "$LDFLAGS"; then
    LDFLAGS="$X_LIBS"
  else
    LDFLAGS="$LDFLAGS $X_LIBS"
  fi
fi

dnl# Checks for header files.
AC_HEADER_SYS_WAIT
AC_CHECK_HEADERS(fcntl.h termios.h \
sys/ioctl.h sys/select.h sys/time.h \
sys/sockio.h sys/byteorder.h malloc.h \
utmpx.h unistd.h bsd/signal.h regex.h \
regexp.h stdarg.h X11/X.h X11/Xlib.h \
X11/Sunkeysym.h X11/Xlocale.h \
)
AC_HEADER_TIME

dnl# Missing typedefs and replacements
AC_TYPE_MODE_T
AC_CHECK_TYPE(off_t, long)
AC_TYPE_PID_T
AC_TYPE_UID_T

dnl# Checks for library functions.
AC_TYPE_SIGNAL
AC_CHECK_FUNCS(atexit _exit unsetenv setutent \
seteuid memmove putenv strsep setresuid setresgid \
memmem usleep snprintf strcasestr strcasechr \
strcasepbrk strrev nl_langinfo \
)

# NOTE:  The following line is NOT NOT NOT NOT NOT a typo!
# If you are having problems with it, libast.m4 is not installed
# or aclocal couldn't find it.  Hence the problem is on YOUR end.
dps_snprintf_oflow()

dnl# Check for math lib.
AC_CHECK_LIB(m, pow)

dnl# Portability checks for various functions
AC_SEARCH_LIBS(login, bsd ucb util)
AC_SEARCH_LIBS(logout, util)
AC_SEARCH_LIBS(getpwuid, sun)

dnl#
dnl# Utility stuff
dnl#
dnl# Did they want debugging?
AC_MSG_CHECKING(for debugging level)
AC_ARG_WITH(debugging, [  --with-debugging[=num]  enable debugging output, num is an integer 0-9], [
              if test "$withval" = "yes"; then
                  withval=4
              else
                  :
              fi
              if test "$withval" != "no"; then 
                  echo "$withval"
                  AC_DEFINE_UNQUOTED(DEBUG, $withval, [Debugging level to compile in.])
              else
                  echo "no debugging"
                  AC_DEFINE_UNQUOTED(DEBUG, 0, [Debugging level to compile in.])
              fi], [echo "4"
                  AC_DEFINE_UNQUOTED(DEBUG, 4, [Debugging level to compile in.])
])

AC_CHECK_FUNC(posix_openpt,
              HAVE_POSIX_OPENPT=yes
             )
AC_CHECK_FUNC(ptsname,
              HAVE_PTSNAME=yes
             )
AC_CHECK_FUNC(grantpt,
              HAVE_GRANTPT=yes
             )
AC_CHECK_FUNC(unlockpt,
              HAVE_UNLOCKPT=yes
             )
dnl# Check for the appropriate pty mechanism
AC_MSG_CHECKING(for pty mechanism)
PTY_MECH=""
if test ! -z "$HAVE_POSIX_OPENPT" -a ! -z "$HAVE_PTSNAME" -a ! -z "$HAVE_GRANTPT" -a ! -z "$HAVE_UNLOCKPT"; then
  AC_DEFINE(HAVE_POSIX_OPENPT, , [Define for posix_openpt() support.])
  PTY_MECH="POSIX $PTY_MECH"
fi
if test -c /dev/ptc ; then
  AC_DEFINE(HAVE_DEV_PTC, , [Define for /dev/ptc support.])
  PTY_MECH="AIX $PTY_MECH"
fi
if test -r /dev/ptmx -a ! -z "$HAVE_PTSNAME" -a ! -z "$HAVE_GRANTPT" -a ! -z "$HAVE_UNLOCKPT"; then
  AC_DEFINE(HAVE_DEV_PTMX, , [Define for /dev/ptmx support.])
  PTY_MECH="SVR4 $PTY_MECH"
fi
if test -c /dev/ptyp128 ; then
  AC_DEFINE(HAVE_SCO_PTYS, , [Define for SCO-style pty support.])
  PTY_MECH="SCO $PTY_MECH"
fi
if test "X$PTY_MECH" = "X"; then
  AC_MSG_RESULT(generic only)
else
  AC_MSG_RESULT($PTY_MECH)
fi
AC_MSG_CHECKING(for pty group)
AC_ARG_WITH(pty-group,
[  --with-pty-group[=gid]  specify the group that should own pty files], [
            if test "$withval" = "yes"; then 
              PTY_GRP_NAME=`ls -1l /dev/pty* 2>/dev/null | head -n 1 | awk '{print $4}'`
              if test ! -z "$PTY_GRP_NAME"; then
                AC_MSG_RESULT($PTY_GRP_NAME)
                AC_DEFINE_UNQUOTED(PTY_GRP_NAME, "$PTY_GRP_NAME", [Group name for pty ownership.])
              else
                AC_MSG_RESULT(unable to determine.  This feature will not be enabled.)
              fi
            elif test "$withval" = "no"; then
              AC_MSG_RESULT(none)
            else
              AC_MSG_RESULT($withval)
              AC_DEFINE_UNQUOTED(PTY_GRP_NAME, "$withval", [Group name for pty ownership.])
            fi
            ], [
            PTY_GRP_NAME=`ls -1l /dev/pty* 2>/dev/null | head -n 1 | awk '{print $4}'`
            if test ! -z "$PTY_GRP_NAME"; then
              AC_MSG_RESULT($PTY_GRP_NAME)
              AC_DEFINE_UNQUOTED(PTY_GRP_NAME, "$PTY_GRP_NAME", [Group name for pty ownership.])
            else
              AC_MSG_RESULT(none)
            fi
])
AC_MSG_CHECKING(for saved uids)
AC_ARG_WITH(saved-uids, [  --with(out)-saved-uids  specify that your OS has (does not have) saved uids, default is to guess], [
            if test "$withval" = "yes"; then
                AC_MSG_RESULT(user specified yes)
                HAVE_SAVED_UIDS=1
            elif test "$withval" = "no"; then
                AC_MSG_RESULT(user specified no)
                HAVE_SAVED_UIDS=0
            fi
])
if test "x$HAVE_SAVED_UIDS" = "x"; then
    case `uname -s` in
        Linux | linux | GNU*Linux)  HAVE_SAVED_UIDS=1 ;;
        HP*UX | hp*ux)              HAVE_SAVED_UIDS=1 ;;
        AIX | aix)                  HAVE_SAVED_UIDS=1 ;;
        Solaris | SunOS | [Ss]un)   HAVE_SAVED_UIDS=1 ;;
        *BSD)                       HAVE_SAVED_UIDS=0 ;;
    esac
    if test "x$HAVE_SAVED_UIDS" = "x"; then
        AC_MSG_RESULT(unknown...assuming no support)
        HAVE_SAVED_UIDS=0
    elif test $HAVE_SAVED_UIDS -eq 1; then
        AC_MSG_RESULT(yes)
        AC_DEFINE(HAVE_SAVED_UIDS, , [Define if the system supports saved UID and GID.])
    else
        AC_MSG_RESULT(no)
    fi
fi
AC_MSG_CHECKING(if strict ICCCM compliance should be enabled)
AC_ARG_ENABLE(strict-icccm,
[  --enable-strict-icccm   compile with strict ICCCM compliant WM icon support], [
              if test "$enableval" = "yes"; then
                  AC_MSG_RESULT(yes)
                  AC_DEFINE(STRICT_ICCCM, , [Define if WM cannot handle icon pixmaps.])
              else
                  AC_MSG_RESULT(no)
              fi
], AC_MSG_RESULT(no))

dnl#
dnl# X LIBRARIES
dnl#
AC_CHECK_LIB(X11, XOpenDisplay, , [
echo "ERROR:  You need libX11 to build Eterm.  Verify that you have libX11.a or";
echo "        libX11.so installed and that it is located in the X libraries";
echo "        directory shown above.  If it is in a different directory, try using";
echo "        the --x-libraries parameter to configure.";
             AC_MSG_ERROR([Fatal:  libX11 not found.])])
AC_CHECK_LIB(Xext, XextAddDisplay, , [
echo "ERROR:  You need libXext to build Eterm.  Verify that you have libXext.a or";
echo "        libXext.so installed and that it is located in the X libraries";
echo "        directory shown above.  If it is in a different directory, try using";
echo "        the --x-libraries parameter to configure.";
             AC_MSG_ERROR([Fatal:  libXext not found.])])

AC_CHECK_LIB(Xext, XShapeQueryExtension, AC_DEFINE(HAVE_X_SHAPE_EXT, , [Define if X shaped window extension is available.]))

dnl#
dnl# FEATURES
dnl#
AC_MSG_CHECKING(if Pablo support is wanted)
AC_ARG_WITH(pablo,
[  --with-pablo[=DIR]      compile with Pablo I/O Tracing support (using Pablo prefix DIR)], [
            if test "$withval" != "no"; then 
              AC_MSG_RESULT(yes)
              if test "$withval" != "yes"; then
                CPPFLAGS="$CPPFLAGS -I${withval}/include"
                LDFLAGS="$LDFLAGS -L${withval}/lib"
              fi
              AC_CHECK_HEADER(IOTrace.h, [
                              AC_CHECK_LIB(PabloTraceExt, initIOTrace, [
                                           AC_DEFINE(IOTRACE, , [Define for Pablo I/O tracing support.])
                                           LIBS="$LIBS -lPabloTraceExt -lPabloTrace"
                                           echo "Pablo support enabled.  Headers in ${withval}/include.  Libraries in ${withval}/lib."
                                           ], [
                                           echo "*** ERROR:  Pablo support was requested but the Pablo libraries could not be"
                                           echo "***         found.  Please check config.log for further information."
                                           echo "***         Pablo support will NOT be included."
                                           ], -lPabloTrace)
                              ], [
                              echo "*** ERROR:  Pablo support was requested but the Pablo header files could not be"
                              echo "***         found.  Please check the value you passed to --with-pablo."
                              echo "***         Pablo support will NOT be included."
                              ])
            else
              AC_MSG_RESULT(no)
            fi], AC_MSG_RESULT(no)
)
AC_MSG_CHECKING(for Escreen support)
AC_ARG_ENABLE(escreen,
[  --disable-escreen       disable Eterm's built-in screen support], [
            if test "$enableval" = "yes"; then
              FOUND_SCREEN=1
              AC_DEFINE(NS_HAVE_SCREEN, 1, [Define for support of the screen text windowing system.])
              AC_MSG_RESULT(yes)
            else
              AC_MSG_RESULT(no)
            fi
          ], [
            FOUND_SCREEN=1
            AC_DEFINE(NS_HAVE_SCREEN, 1, [Define for support of the screen text windowing system.])
            AC_MSG_RESULT(yes)
          ]
)
AC_MSG_CHECKING(for Etwin support)
AC_ARG_ENABLE(etwin,
[  --enable-etwin          enable Eterm's built-in twin support], [
            if test "$enableval" = "yes"; then 
              AC_CHECK_LIB(Tw, Tw_Open, FOUND_LIBTWIN=1, FOUND_LIBTWIN=0, $GRLIBS)
              if test "$FOUND_LIBTWIN" -ne "1"; then
                AC_WARN(*** Twin support has been disabled because libTw was not found ***)
              else
                AC_DEFINE(NS_HAVE_TWIN, 1, [Define for support of the twin text windowing system.])
                LIBS="$LIBS -lTw"
              fi
            else
              AC_MSG_RESULT(no)
            fi], AC_MSG_RESULT(no)
)

if test ! -z "$FOUND_SCREEN" || test ! -z "$FOUND_LIBTWIN"; then
    AC_DEFINE(ESCREEN, , [Define for Escreen support.])
    AC_MSG_CHECKING(for Escreen startup effects)
    AC_ARG_ENABLE(escreen-fx, [  --enable-escreen-fx     enable Escreen startup effects], [
            if test "$enableval" = "yes"; then
                AC_MSG_RESULT(yes)
                AC_DEFINE(ESCREEN_FX, , [Define to enable Escreen startup effects.])
            else
                AC_MSG_RESULT(no)
            fi
        ], [
            AC_MSG_RESULT(no)
        ]
    )
fi

AC_MSG_CHECKING(if profiling macros should be included)
AC_ARG_ENABLE(profile,
[  --enable-profile        compile with code profiling macros enabled], [
            if test "$enableval" = "yes"; then 
              AC_MSG_RESULT(yes)
              AC_DEFINE(ENABLE_PROFILE, , [Define to enable code profiling macros.])
            else
              AC_MSG_RESULT(no)
            fi], AC_MSG_RESULT(no)
)
AC_MSG_CHECKING(for pixmap support)
AC_ARG_WITH(imlib,
[  --with-imlib[=DIR]      compile with Imlib support (Imlib residing in DIR/lib) (default)], [
            if test "$withval" != "no"; then 
              AC_MSG_RESULT(yes)
              if test "$withval" != "yes"; then
                CPPFLAGS="$CPPFLAGS -I${withval}/include"
                LDFLAGS="$LDFLAGS -L${withval}/lib"
              fi
              USE_IMLIB=1
            else
              AC_MSG_RESULT(no)
              GRLIBS=""
              USE_IMLIB=0
              AC_WARN(*** Pixmap support has been disabled at your request.  This ***)
              AC_WARN(*** is generally a bad idea since it really doesn't save    ***)
              AC_WARN(*** much memory.  And the developers almost never test that ***)
              AC_WARN(*** configuration.  So I'd reconsider if I were you....     ***)
            fi], [AC_MSG_RESULT(yes)
                USE_IMLIB=1
])
if test $USE_IMLIB -eq 1 ; then
  AC_CHECK_LIB(dl, dlopen, GRLIBS="-ldl", , $GRLIBS)
  AC_CHECK_LIB(ttf, TT_Init_FreeType, GRLIBS="-lttf $GRLIBS", , $GRLIBS)
  AC_CHECK_LIB(Imlib2, imlib_create_image, [
               GRLIBS="-lImlib2 $GRLIBS"
               AC_DEFINE(HAVE_LIBIMLIB2, , [Define if Imlib2 is present.])
               AC_DEFINE(PIXMAP_SUPPORT, , [Define if pixmap support is desired.])
               AC_DEFINE(BACKING_STORE, , [Define if backing store is desired.])
              ], [
               AC_WARN(*** Pixmap support has been disabled because Imlib2 was not found ***)
               AC_WARN(*** or could not be linked.  Eterm should still work, but it will ***)
               AC_WARN(*** not be very happy.  Check config.log for more detailed        ***)
               AC_WARN(*** information on why my attempt to link with Imlib2 failed.     ***)
              ], $GRLIBS)

fi

AC_MSG_CHECKING(for transparency support)
AC_ARG_ENABLE(trans,
[  --enable-trans          compile with transparency support], [
            if test "$enableval" != "no"; then 
              AC_MSG_RESULT(yes)
              AC_DEFINE(PIXMAP_OFFSET, , [Define for pseudo-transparency support.])
            else
              AC_MSG_RESULT(no)
            fi], [AC_MSG_RESULT(yes)
                AC_DEFINE(PIXMAP_OFFSET, , [Define for pseudo-transparency support.])
])

dnl#
dnl# MMX support
dnl#
AC_MSG_CHECKING(for MMX support)
HAVE_MMX=""
AC_ARG_ENABLE(mmx, [  --enable-mmx            enable MMX assembly routines], [
                  test "x$enableval" = "xyes" && HAVE_MMX="yes"
              ], [
                  case $host_cpu in
                      i*86)
                          grep mmx /proc/cpuinfo >/dev/null 2>&1 && HAVE_MMX="yes"
                          ;;
                  esac
              ])
if test "x$HAVE_MMX" = "xyes"; then
    AC_MSG_RESULT([yes (32-bit)])
    AC_DEFINE(HAVE_MMX, , [Define for 32-bit MMX support.])
else
    AC_MSG_RESULT([no (no MMX detected)])
fi
AM_CONDITIONAL(HAVE_MMX, test "x$HAVE_MMX" = "xyes")

dnl#
dnl# SSE2 support
dnl#
AC_MSG_CHECKING(for SSE2 support)
HAVE_SSE2=""
AC_ARG_ENABLE(sse2, [  --enable-sse2            enable SSE2 assembly routines], [
                  test "x$enableval" = "xyes" && HAVE_SSE2="yes"
              ], [
                  case $host_cpu in
                      x86_64)
                          grep sse2 /proc/cpuinfo >/dev/null 2>&1 && HAVE_SSE2="yes"
                          ;;
                  esac
              ])
if test "x$HAVE_SSE2" = "xyes"; then
    AC_MSG_RESULT([yes])
    AC_DEFINE(HAVE_SSE2, , [Define for 64-bit SSE2 support.])
else
    AC_MSG_RESULT([no (no SSE2 detected)])
fi
AM_CONDITIONAL(HAVE_SSE2, test "x$HAVE_SSE2" = "xyes")

dnl#
dnl# LibAST
dnl#
LIBAST_MIN=5
AC_CHECK_PROG(LIBAST_CONFIG, libast-config, libast-config, false)
if test "$LIBAST_CONFIG" = "false"; then
    FOUND_LIBAST=0
else
    FOUND_LIBAST=`$LIBAST_CONFIG --version | awk -F. '{print $2}'`
    CPPFLAGS="$CPPFLAGS `$LIBAST_CONFIG --cppflags`"
    LDFLAGS="$LDFLAGS `$LIBAST_CONFIG --ldflags`"
    GRLIBS="$GRLIBS `$LIBAST_CONFIG --libs`"
fi
AC_CHECK_LIB(ast, libast_set_program_name, , FOUND_LIBAST=0, $GRLIBS)

if test "$FOUND_LIBAST" -lt "$LIBAST_MIN"; then
  echo "ERROR:  You need LibAST 0.$LIBAST_MIN or higher to build Eterm.  If you already have it,";
  echo "        you may have it installed in a strange place, or you may need to run";
  echo "        /sbin/ldconfig.  If you don't have it, I can download it for you.";
  echo "        Shall I retrieve and build LibAST now (y/n)?";
  read ANS
  if test "x$ANS" = "xy" -o "x$ANS" = "xyes" -o "x$ANS" = "xY" -o "x$ANS" = "xYES"; then
      # Download from CVS server
      CVSROOT=":pserver:anonymous@cvs.enlightenment.sourceforge.net:/cvsroot/enlightenment"
      test -f $HOME/.cvspass || touch $HOME/.cvspass
      grep $CVSROOT $HOME/.cvspass >/dev/null 2>&1 || cvs -d $CVSROOT login
      cvs -z3 -d $CVSROOT co -d libast eterm/libast
      (cd libast && ./autogen.sh $ac_configure_args && make install && cd .. && rm -rf libast)
      if test $? -ne 0; then
          echo 'ERROR:  Unable to auto-get libast, sorry.' 1>&2
          exit 1
      fi
      FOUND_LIBAST=1
      AC_CHECK_PROG(LIBAST_CONFIG, libast-config, libast-config, false)
      test "$LIBAST_CONFIG" = "false" && FOUND_LIBAST=0
  fi
fi
if test "$FOUND_LIBAST" -ge "$LIBAST_MIN"; then
  if test ! -z "$LIBAST_CONFIG"; then
    #GRLIBS="-last $GRLIBS"
    AC_DEFINE(HAVE_LIBAST, , [Define if LibAST is present.])
    test "$prefix" = "NONE" && prefix="`$LIBAST_CONFIG --prefix`"
    SUPPORT_FLAGS="`$LIBAST_CONFIG --support`"
    for i in $SUPPORT_FLAGS ; do
      case $i in
        MMX)
          AC_DEFINE(LIBAST_MMX_SUPPORT, 1, [Defined as 1 if LibAST has MMX support.])
          ;;
        X11)
          AC_DEFINE(LIBAST_X11_SUPPORT, 1, [Defined as 1 if LibAST has X11 support.])
          ;;
        Imlib2)
          AC_DEFINE(LIBAST_IMLIB2_SUPPORT, 1, [Defined as 1 if LibAST has Imlib2 support.])
          ;;
      esac
    done
  fi
else
  AC_MSG_ERROR(Fatal:  libast not found.)
fi

AC_MSG_CHECKING(for utmp support)
AC_ARG_ENABLE(utmp,
[  --enable-utmp           compile with utmp support], [
            if test "$enableval" != "no"; then 
              AC_MSG_RESULT(yes)
              UTMP=1
              AC_DEFINE(UTMP_SUPPORT, , [Define for utmp logging support.])
            else
              AC_MSG_RESULT(no)
              UTMP=0
            fi], [AC_MSG_RESULT(yes)
                AC_DEFINE(UTMP_SUPPORT, , [Define for utmp logging support.])
                UTMP=1
])
if test $UTMP -eq 1; then
  AC_CHECK_LIB(utempter, addToUtmp)
fi
AC_MSG_CHECKING(for backspace key configuration)
AC_ARG_WITH(backspace,
[  --with-backspace=KEY    force backspace to send KEY (KEY is \"bs\" for ^H, \"del\" for ^?, \"auto\" to check stty)], [
            if test "$withval" = "bs"; then 
              AC_MSG_RESULT(forcing Backspace to send Ctrl-H)
              AC_DEFINE(FORCE_BACKSPACE, , [Define to force Backspace to send ^H.])
            elif test "$withval" = "del"; then 
              AC_MSG_RESULT(forcing Backspace to send Ctrl-?)
              AC_DEFINE(FORCE_DELETE, , [Define to force Backspace to send ^?.])
            elif test "$withval" = "detect" -o "$withval" = "auto"; then 
              AC_MSG_RESULT(attempt to auto-detect)
            else
              AC_MSG_RESULT(default)
              AC_DEFINE(FORCE_BACKSPACE, , [Define to force Backspace to send ^H.])
            fi], [AC_MSG_RESULT(default)
                AC_DEFINE(FORCE_BACKSPACE, , [Define to force Backspace to send ^H.])
])
AC_MSG_CHECKING(for delete key configuration)
AC_ARG_WITH(delete,
[  --with-delete=SETTING   force delete to SETTING (\"server\" to use the X server value always,
                          \"execute\" to send the old default execute escape sequence, \"del\" for ^?,
                          or a quoted string to use a specific string)], [
            if test "$withval" = "server"; then
              AC_MSG_RESULT(X server value)
              AC_DEFINE(NO_DELETE_KEY, , [Define to use the X server value for Delete.])
            elif test "$withval" = "no"; then
              AC_MSG_RESULT(X server value)
              AC_DEFINE(NO_DELETE_KEY, , [Define to use the X server value for Delete.])
            elif test "$withval" = "execute"; then
              AC_MSG_RESULT(send execute sequence)
            elif test "$withval" = "del"; then
              AC_MSG_RESULT(send Ctrl-?)
              AC_DEFINE_UNQUOTED(KS_DELETE, "\177", [Define to force Delete to send ^?.])
            else
              AC_MSG_RESULT(forcing Delete to send "$withval")
              AC_DEFINE_UNQUOTED(KS_DELETE, "$withval", [Define for specific Delete escape sequence.])
            fi], AC_MSG_RESULT(default)
)
XTERM_HOME="\033\133H"
LINUX_HOME="\033\1331~"
AC_MSG_CHECKING(for home key configuration)
AC_ARG_WITH(home,
[  --with-home=SEQUENCE    specify which sequence to use for the Home key ("xterm", "linux", or "vt102")], [
            if test "$withval" = "xterm"; then 
              AC_MSG_RESULT(emulate xterm)
              AC_DEFINE_UNQUOTED(KS_HOME, "$XTERM_HOME", [Emulate xterm for Home key.])
            elif test "$withval" = "linux"; then 
              AC_MSG_RESULT(emulate linux console/vt220)
              AC_DEFINE_UNQUOTED(KS_HOME, "$LINUX_HOME", [Emulate Linux console for Home key.])
            else
              AC_MSG_RESULT(default vt102)
            fi], AC_MSG_RESULT(default)
)
XTERM_END="\033\133F"
LINUX_END="\033\1334~"
AC_MSG_CHECKING(for end key configuration)
AC_ARG_WITH(end,
[  --with-end=SEQUENCE     specify which sequence to use for the End key ("xterm", "linux", or "vt102")], [
            if test "$withval" = "xterm"; then 
              AC_MSG_RESULT(emulate xterm)
              AC_DEFINE_UNQUOTED(KS_END, "$XTERM_END", [Emulate xterm for End key.])
            elif test "$withval" = "linux"; then 
              AC_MSG_RESULT(emulate linux console/vt220)
              AC_DEFINE_UNQUOTED(KS_END, "$LINUX_END", [Emulate Linux console for End key.])
            else
              AC_MSG_RESULT(default vt102)
            fi], AC_MSG_RESULT(default)
)
AC_MSG_CHECKING(if mousewheel support should be enabled)
AC_ARG_WITH(mousewheel,
[  --without-mousewheel    disable built-in mousewheel support], [
            if test "$withval" = "no"; then 
              AC_MSG_RESULT(no)
            else
              AC_MSG_RESULT(yes)
              AC_DEFINE(MOUSEWHEEL, , [Define for built-in mousewheel support.])
            fi], [AC_MSG_RESULT(yes)
                AC_DEFINE(MOUSEWHEEL, , [Define for built-in mousewheel support.])
])

AC_MSG_CHECKING(for automatic encoding)
AC_ARG_ENABLE(auto-encoding,
[  --enable-auto-encoding  compile with runtime automatic encoding setting],
             if test "$enableval" = "no"; then
               AC_MSG_RESULT(no)
             else
               AC_MSG_RESULT(yes)
               AC_DEFINE(AUTO_ENCODING, 1, [Define for automatic character encoding.])
             fi, AC_MSG_RESULT(yes)
                 AC_DEFINE(AUTO_ENCODING, 1, [Define for automatic character encoding.])
)

MULTICHAR_ENCODING=""
AC_MSG_CHECKING(for multi-charset support)
AC_ARG_ENABLE(multi-charset,
[  --enable-multi-charset  compile with multibyte font support], [
            if test "$enableval" = "no"; then
              MULTI_CHARSET_TYPE=""
            elif test "$enableval" = "yes"; then
              MULTI_CHARSET_TYPE="unicode"
            else
              MULTI_CHARSET_TYPE="$enableval"
            fi], [
            if (xlsfonts 2>/dev/null | grep 10646 >&5 2>&5); then
              MULTI_CHARSET_TYPE="unicode"
            else
              MULTI_CHARSET_TYPE=""
            fi
           ])

if test "x$MULTI_CHARSET_TYPE" != "x"; then
  if test "$MULTI_CHARSET_TYPE" = "kanji"; then
    AC_MSG_RESULT(kanji)
    AC_DEFINE(MULTI_CHARSET, , [Define for multi-byte character set support.])
    DEF_FONT_IDX=0
    MULTICHAR_ENCODING="eucj"
    FONT0="fixed"
    FONT1="8x16"
    FONT2="9x18"
    FONT3="12x24"
    FONT4="13x26"
    MFONT0="k14"
    MFONT1="jiskan16"
    MFONT2="jiskan18"
    MFONT3="jiskan24"
    MFONT4="jiskan26"
  elif test "$MULTI_CHARSET_TYPE" = "euc-kr" -o "$MULTI_CHARSET_TYPE" = "euckr"; then 
    AC_MSG_RESULT(euckr)
    AC_DEFINE(MULTI_CHARSET, , [Define for multi-byte character set support.])
    DEF_FONT_IDX=1
    MULTICHAR_ENCODING="euckr"
    FONT0="7x14"
    FONT1="8x16"
    FONT2="9x18"
    FONT3="10x20"
    FONT4="12x24"
    MFONT0="-*-gulim-medium-r-normal--14-*-*-*-*-140-ksc5601.1987-0"
    MFONT1="-*-gulim-medium-r-normal--16-*-*-*-*-160-ksc5601.1987-0"
    MFONT2="-*-gulim-medium-r-normal--18-*-*-*-*-180-ksc5601.1987-0"
    MFONT3="-*-gulim-medium-r-normal--20-*-*-*-*-200-ksc5601.1987-0"
    MFONT4="-*-gulim-medium-r-normal--24-*-*-*-*-240-ksc5601.1987-0"
  elif test "$MULTI_CHARSET_TYPE" = "unicode" -o "$MULTI_CHARSET_TYPE" = "utf-8" -o "$MULTI_CHARSET_TYPE" = "utf8"; then 
    AC_MSG_RESULT(ISO-10646)
    AC_DEFINE(MULTI_CHARSET, , [Define for multi-byte character set support.])
    DEF_FONT_IDX=2
    MULTICHAR_ENCODING="iso-10646"
    MFONT0="-misc-fixed-medium-r-normal--7-70-75-75-c-50-iso10646-1"
    MFONT1="-misc-fixed-medium-r-normal--10-100-75-75-c-60-iso10646-1"
    MFONT2="-misc-fixed-medium-r-semicondensed--13-120-75-75-c-60-iso10646-1"
    MFONT3="-misc-fixed-medium-r-normal--14-130-75-75-c-70-iso10646-1"
    MFONT4="-misc-fixed-medium-r-normal--15-140-75-75-c-90-iso10646-1"
    FONT0="5x7"
    FONT1="6x10"
    FONT2="fixed"
    FONT3="8x13"
    FONT4="9x15"
  elif test "$MULTI_CHARSET_TYPE" = "big5"; then
    AC_MSG_RESULT(big5)
    AC_DEFINE(MULTI_CHARSET, , [Define for multi-byte character set support.])
    DEF_FONT_IDX=0
    MULTICHAR_ENCODING="big5"
    FONT0="8x16"
    FONT1="9x18"
    FONT2="10x20"
    FONT3="12x24"
    FONT4="13x26"
    MFONT0="-*-ming-medium-r-normal--16-*-*-*-c-*-big5-0"
    MFONT1="-*-ming-medium-r-normal--18-*-*-*-c-*-big5-0"
    MFONT2="-*-ming-medium-r-normal--20-*-*-*-c-*-big5-0"
    MFONT3="-*-ming-medium-r-normal--24-*-*-*-c-*-big5-0"
    MFONT4="-*-ming-medium-r-normal--26-*-*-*-c-*-big5-0"
  else
    MULTI_CHARSET_TYPE=""
  fi
fi
if test "x$MULTI_CHARSET_TYPE" = "x"; then
  AC_MSG_RESULT(no)
  DEF_FONT_IDX=2
  MULTICHAR_ENCODING=none
  MFONT0=none
  MFONT1=none
  MFONT2=none
  MFONT3=none
  MFONT4=none
  FONT0="5x7"
  FONT1="6x10"
  FONT2="fixed"
  FONT3="8x13"
  FONT4="9x15"
fi

AC_DEFINE_UNQUOTED(DEF_FONT_IDX, $DEF_FONT_IDX, [Default font index.])
AC_DEFINE_UNQUOTED(FONT0, "$FONT0", [Font 0])
AC_DEFINE_UNQUOTED(FONT1, "$FONT1", [Font 1])
AC_DEFINE_UNQUOTED(FONT2, "$FONT2", [Font 2])
AC_DEFINE_UNQUOTED(FONT3, "$FONT3", [Font 3])
AC_DEFINE_UNQUOTED(FONT4, "$FONT4", [Font 4])
AC_SUBST(DEF_FONT_IDX)
AC_SUBST(FONT0)
AC_SUBST(FONT1)
AC_SUBST(FONT2)
AC_SUBST(FONT3)
AC_SUBST(FONT4)
export FONT0 FONT1 FONT2 FONT3 FONT4 DEF_FONT_IDX

AC_DEFINE_UNQUOTED(MULTICHAR_ENCODING, "$MULTICHAR_ENCODING", [Multi-byte character encoding style.])
AC_DEFINE_UNQUOTED(MFONT0, "$MFONT0", [Multibyte Font 0])
AC_DEFINE_UNQUOTED(MFONT1, "$MFONT1", [Multibyte Font 1])
AC_DEFINE_UNQUOTED(MFONT2, "$MFONT2", [Multibyte Font 2])
AC_DEFINE_UNQUOTED(MFONT3, "$MFONT3", [Multibyte Font 3])
AC_DEFINE_UNQUOTED(MFONT4, "$MFONT4", [Multibyte Font 4])
AC_SUBST(MULTICHAR_ENCODING)
AC_SUBST(MFONT0)
AC_SUBST(MFONT1)
AC_SUBST(MFONT2)
AC_SUBST(MFONT3)
AC_SUBST(MFONT4)
export MFONT0 MFONT1 MFONT2 MFONT3 MFONT4 MULTICHAR_ENCODING

AC_MSG_CHECKING(for XIM support)
AC_ARG_ENABLE(xim,
[  --enable-xim            compile with XIM support (default)], [
            if test "$enableval" = "no"; then 
              AC_MSG_RESULT(no)
	      XIM="FALSE"
            else
              AC_MSG_RESULT(yes)
	      AC_DEFINE(USE_XIM, , [Define for X Input Method support.])
	      XIM="TRUE"
            fi], [AC_MSG_RESULT(yes)
	        AC_DEFINE(USE_XIM, , [Define for X Input Method support.])
	        XIM="TRUE"
])

# check X11R6 for XIM
if test "$XIM" = "TRUE"; then
  AC_CHECK_LIB(X11, XRegisterIMInstantiateCallback, AC_DEFINE(USE_X11R6_XIM, , [Define if we have X11R6 XIM.]))
fi

# Check for XResource extension
AC_CHECK_HEADER([X11/extensions/XRes.h],
                [
                    AC_DEFINE([HAVE_X11_EXTENSIONS_XRES_H], [], [Define if we have X11/extensions/XRes.h])
                ], [],
                [
#ifdef HAVE_X11_X_H
#  include <X11/X.h>
#endif
#ifdef HAVE_X11_XLIB_H
#  include <X11/Xlib.h>
#endif
                ])
AC_CHECK_LIB(XRes, XResQueryExtension,
             [
                 GRLIBS="$GRLIBS -lXRes"
                 AC_DEFINE(HAVE_XRES_EXT, , [Define if we have the XResource extension.])
             ])

AC_MSG_CHECKING(for Greek keyboard support)
AC_ARG_ENABLE(greek,
[  --enable-greek          compile with support for Greek keyboards], [
            if test "$enableval" = "yes"; then 
              AC_MSG_RESULT(yes)
              AC_DEFINE(GREEK_SUPPORT, , [Define for Greek keyboard support.])
            else
              AC_MSG_RESULT(no)
            fi], AC_MSG_RESULT(no)
)

AC_ARG_WITH(terminfo,
[  --without-terminfo      do not compile the Eterm terminfo file], [
            if test "$withval" = "no"; then 
              TIC=true
            fi
])

AC_ARG_WITH(theme-update,
[  --with-theme-update     existing themes will be forceably removed and new ones installed], [
            if test "$withval" = "yes"; then 
              REMOVE_THEMES=yes
            else
              REMOVE_THEMES=no
            fi], [REMOVE_THEMES=no
])
AC_SUBST(REMOVE_THEMES)

dnl# AC_MSG_CHECKING(which threads library to use)
dnl# AC_ARG_WITH(threads,
dnl# [  --with-threads[=STYLE]  compile with threads support, STYLE is either "posix" or blank
dnl#                           (disabled by default)],
dnl#             case $withval in
dnl#               [yes | posix )]
dnl#                 AC_MSG_RESULT(POSIX)
dnl#                 THREADS_LIB=posix
dnl#                 ;;
dnl#               [* )]
dnl#                 AC_MSG_RESULT(none)
dnl#                 ;;
dnl#             esac
dnl#  , AC_MSG_RESULT(none))
dnl# if test "$THREADS_LIB" = "posix"; then
dnl#   AC_CHECK_LIB(pthread, pthread_create, CPPFLAGS="$CPPFLAGS -D_REENTRANT" ; THREADLIBS="-lpthread"
dnl#                                        AC_DEFINE(USE_POSIX_THREADS)
dnl#                                        AC_DEFINE(MUTEX_SYNCH)
dnl#                , , -D_REENTRANT -L/usr/lib -L/lib -L/usr/local/lib)
dnl# fi

AC_PREFIX(Eterm)
AC_PREFIX(gcc)
if test "$prefix" = "NONE"; then
  prefix=$ac_default_prefix
fi
if test "$exec_prefix" = "NONE"; then
  exec_prefix=$prefix
fi

dnl Stack Tracing toys
AC_ARG_ENABLE(stack-trace, [  --disable-stack-trace   disable stack trace on abnormal termination], [
  if test "$enableval" = "no"; then
    AC_DEFINE(NO_STACK_TRACE, , [Define to disable stack trace on abnormal termination.])
    NO_STACK_TRACE=1
  fi
])

if test "$NO_STACK_TRACE" != "1"; then
  AC_PATH_PROG(DBX, dbx, no)
  if test "$DBX" != "no"; then
    AC_DEFINE_UNQUOTED(DBX, "$DBX", [Define for dbx support.])
  fi
  AC_PATH_PROG(GDB, gdb, no)
  if test "$GDB" != "no"; then
    AC_DEFINE_UNQUOTED(GDB, "$GDB", [Define for gdb support.])
  fi
  AC_PATH_PROG(PSTACK, pstack, no, $PATH:/usr/proc/bin:/usr/sbin)
  if test "$PSTACK" != "no"; then
    AC_DEFINE_UNQUOTED(PSTACK, "$PSTACK", [Define for pstack support.])
  fi
  AC_CHECK_LIB(cl, U_STACK_TRACE, LIBS="$LIBS -lcl")
fi

AC_MSG_CHECKING(for Linux 2.1 or higher)
OS_NAME=`uname -s`
if test "$OS_NAME" = "Linux"; then
  OS_REV=`uname -r`
  OS_MAJOR=`echo $OS_REV | cut -d. -f1`
  OS_MINOR=`echo $OS_REV | cut -d. -f2`
  if test "$OS_MAJOR" -ge "2" -a "$OS_MINOR" -ge "1"; then
    AC_MSG_RESULT(yes)
    AC_DEFINE(NEED_LINUX_HACK, , [Define if uid hack needed to open X display.])
  else
    AC_MSG_RESULT(no)
  fi
else
  AC_MSG_RESULT(no)
fi

dnl# Enable/disable name reporting escape sequences
AC_MSG_CHECKING(if we should enable name reporting escape sequences)
AC_ARG_ENABLE(name-reporting-escapes, [ --enable-name-reporting-escapes  Specify this to enable title/icon name reporting], [
              if test "$enableval" = "yes"; then
                  AC_MSG_RESULT(yes)
                  AC_DEFINE(ENABLE_NAME_REPORTING_ESCAPES, 1, [Defined to enable title/icon name reporting escape sequences.])
              else
                  AC_MSG_RESULT(no)
              fi
], [AC_MSG_RESULT(no)])

dnl# Enable/disable humor
AC_ARG_WITH(sense-of-humor, [  --without-sense-of-humor   Specify this if you have no sense of humor], [
            if test "$withval" = "no"; then
              HUMOR=none
            fi
])

if test -z "$HUMOR"; then
AC_CHECK_LIB(Kenny, life_signs, , [
  echo "  Oh my god, they killed Kenny!  You bastards!"
  AC_DEFINE(HAVE_HUMOR, 1, [Defined if user has a sense of humor.])
])
fi

CONFIG_SEARCH_PATH=`eval eval eval eval eval echo "${sysconfdir}/$PACKAGE/themes:/etc/$PACKAGE/themes:${datadir}/$PACKAGE/themes"`
CONFIG_SEARCH_PATH=`eval eval eval eval eval echo "$CONFIG_SEARCH_PATH:${sysconfdir}/$PACKAGE:/etc/$PACKAGE:${datadir}/$PACKAGE"`
AC_DEFINE_UNQUOTED(CONFIG_SEARCH_PATH, "~/.Eterm/themes:~/.Eterm:$CONFIG_SEARCH_PATH", [Config file/theme search path.])
# Do replacements on theme files and such.  All variables to be replaced
# by this routine MUST be exported before the awk script runs.
if test "X$PKGDATADIR" = "X" ; then
  PKGDATADIR=`eval eval eval eval echo "$datadir/$PACKAGE"`
fi
export PKGDATADIR
AC_DEFINE_UNQUOTED(PKGDATADIR, "$PKGDATADIR", [Package data directory.])
if test "$GDB" != "no"; then
  GDB_CMD_FILE="$PKGDATADIR/gdb.scr"
  AC_DEFINE_UNQUOTED(GDB_CMD_FILE, "$GDB_CMD_FILE", [gdb command script.])
else
  GDB_CMD_FILE=""
fi
AC_SUBST(GDB_CMD_FILE)

CPPFLAGS=`eval eval eval eval eval echo "-I$includedir -I$prefix/include $CPPFLAGS"`
CPPFLAGS=`echo $CPPFLAGS | tr ' ' '\n' | uniq | grep -v NONE | tr '\n' ' '`
CFLAGS=${CFLAGS--O}
LDFLAGS=`eval eval eval eval eval echo "-L$libdir -L$prefix/lib ${LDFLAGS--O}"`
LDFLAGS=`echo $LDFLAGS | tr ' ' '\n' | uniq | grep -v NONE | tr '\n' ' '`
LIBS="$GRLIBS $X_PRE_LIBS $LIBS $X_EXTRA_LIBS"

AC_SUBST(CC)
AC_SUBST(CFLAGS)
AC_SUBST(CPPFLAGS)
AC_SUBST(LDFLAGS)
AC_SUBST(LIBS)
AC_SUBST(THREADLIBS)

basedir=.

AM_CONFIG_HEADER(config.h)

AC_OUTPUT([Makefile bg/Makefile doc/Makefile utils/Makefile pix/Makefile
          src/Makefile themes/Makefile])

# Mega evals to make sure all the relative variables get resolved.
BINDIR=`eval eval eval eval echo ${bindir}`
export BINDIR bindir prefix exec_prefix datadir PACKAGE

# The awk-fu below replaces @VAR@ in theme config files with the corresponding
# environment variables, kinda like AC_OUTPUT above.
changequote(%&, &%)dnl
%&
for i in utils/kEsetroot themes/Eterm/theme.cfg themes/auto/theme.cfg themes/Escreen/theme.cfg \
         themes/trans/theme.cfg ; do
  echo creating $i
  $AWK '{if (match($0,/@[A-Za-z0-9_]*@/)) {
           i=ENVIRON[substr($0,RSTART+1,RLENGTH-2)];
           outp=$0;
           gsub(/@[A-Za-z0-9_]*@/,i,outp);
           print outp;
         } else {
           print $0;
         }
        }' $i.in > $i
done
&%
changequote([, ])dnl

dnl If we're not building with Imlib support, don't build Esetroot
if test -z "$GRLIBS"; then
  sed -e 's/^bin_PROGRAMS.*$//g' $srcdir/doc/Makefile > $srcdir/doc/Makefile.new
  mv $srcdir/doc/Makefile.new $srcdir/doc/Makefile
fi

echo "
$PACKAGE $VERSION
Configuration:
--------------

  Source code location:       $srcdir
  Host System Type:           $host
  Preprocessor:               $CPP $CPPFLAGS
  Compiler:                   $CC $CFLAGS
  Linker:                     $CC $LDFLAGS $LIBS
  Install path:               $prefix

  See src/feature.h for further configuration information.

  Now type 'make' to build $PACKAGE $VERSION.
"
