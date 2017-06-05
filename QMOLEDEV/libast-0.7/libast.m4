dnl#####################################################################
dnl# Autoconf m4 macros for LibAST
dnl# $Id: libast.m4,v 1.14 2005/12/22 23:28:55 mej Exp $
dnl#####################################################################

dnl#
dnl# Check for LibAST and support components
dnl#    - arg 1 is the env variable to set
dnl#    - arg 2 is the LIBS variable to use
dnl#
AC_DEFUN([AST_CHECK_LIBAST], [
    AC_CHECK_PROG(LIBAST_CONFIG, libast-config, libast-config, false)
    if test "$LIBAST_CONFIG" = "false"; then
        $1=0
    else
        $1=1
        CPPFLAGS="$CPPFLAGS `$LIBAST_CONFIG --cppflags`"
        LDFLAGS="$LDFLAGS `$LIBAST_CONFIG --ldflags`"
    fi
    AC_CHECK_HEADERS(libast.h, [
        AC_CHECK_LIB(ast, libast_malloc, $1=1, [ AC_CHECK_LIB(ast, spifmem_malloc, $1=0, ${$2}) ]
    ], $1=0)

    if test "${$1}" -ne "1"; then
        echo "ERROR:  You need the LibAST package to build Eterm.  If you already have it,";
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
            $1=1
            AC_CHECK_PROG(LIBAST_CONFIG, libast-config, libast-config, false)
            test "$LIBAST_CONFIG" = "false" && $1=0
        fi
    fi
    if test "${$1}" -eq "1"; then
        if test ! -z "$LIBAST_CONFIG"; then
            $2="-last ${$2}"
            AC_DEFINE([HAVE_LIBAST], [1], [Define if the LibAST library is present.])
            test "$prefix" = "NONE" && prefix="`$LIBAST_CONFIG --prefix`"
            SUPPORT_FLAGS="`$LIBAST_CONFIG --support`"
            for i in $SUPPORT_FLAGS ; do
                case $i in
                    MMX)
                        AC_DEFINE([LIBAST_MMX_SUPPORT], [1], [Defined if LibAST has MMX support.])
                        ;;
                    X11)
                        AC_DEFINE([LIBAST_X11_SUPPORT], [1], [Defined if LibAST has X11 support.])
                        ;;
                    Imlib2)
                        AC_DEFINE([LIBAST_IMLIB2_SUPPORT], [1], [Defined if LibAST has Imlib2 support.])
                        ;;
                esac
            done
        fi
    fi
])

dnl#
dnl# LibAST macro for determining integer types by size
dnl#
AC_DEFUN([AST_SIZE_TYPE], [
    BIT_SIZE=[$1]
    BYTE_SIZE=`expr $BIT_SIZE '/' 8`
    case $BYTE_SIZE in
        $ac_cv_sizeof_char)       eval INT_${BIT_SIZE}_TYPE=char ;;
        $ac_cv_sizeof_short)      eval INT_${BIT_SIZE}_TYPE=short ;;
        $ac_cv_sizeof_int)        eval INT_${BIT_SIZE}_TYPE=int ;;
        $ac_cv_sizeof_long)       eval INT_${BIT_SIZE}_TYPE=long ;;
        $ac_cv_sizeof_long_long)  eval INT_${BIT_SIZE}_TYPE="'long long'" ;;
    esac
    test -z "`eval echo '$'INT_${BIT_SIZE}_TYPE`" && eval INT_${BIT_SIZE}_TYPE=long
])

dnl#
dnl# LibAST macro for determining regexp support
dnl#    - arg 1 is the name of the env var to use
dnl#
AC_DEFUN([AST_REGEXP_SUPPORT], [
    if test "${$1}" != "no"; then
        if test "${$1}" = "pcre" -o "${$1}" = "yes" ; then
            GOT_PCRE_HEADER=0
            GOT_PCRE_LIB=0
            AC_CHECK_HEADERS(pcre.h pcre/pcre.h, [
                                 GOT_PCRE_HEADER=1
                                 break
                             ])
            AC_SEARCH_LIBS(pcre_compile, pcre, [GOT_PCRE_LIB=1])
            if test $GOT_PCRE_HEADER -eq 1 -a $GOT_PCRE_LIB -eq 1 ; then
                AC_DEFINE([LIBAST_REGEXP_SUPPORT_PCRE], [1], [Build LibAST with PCRE support.])
                LIBAST_REGEXP_SUPPORT="regexp-pcre"
                $1="pcre"
            else
                $1="yes"
            fi
        fi
        if test "${$1}" = "posix" -o "${$1}" = "yes" ; then
            GOT_POSIXREGEXP_HEADER=0
            GOT_POSIXREGEXP_LIB=0
            AC_CHECK_HEADERS(regex.h, [
                                 GOT_POSIXREGEXP_HEADER=1
                                 break
                             ])
            AC_SEARCH_LIBS(regcomp, posix regexp regex re, [GOT_POSIXREGEXP_LIB=1])
            if test $GOT_POSIXREGEXP_HEADER -eq 1 -a $GOT_POSIXREGEXP_LIB -eq 1 ; then
                AC_DEFINE([LIBAST_REGEXP_SUPPORT_POSIX], [1], [Build LibAST with POSIX-style regexp support.])
                LIBAST_REGEXP_SUPPORT="regexp-posix"
                $1="posix"
            else
                $1="yes"
            fi
        fi
        if test "${$1}" = "bsd" -o "${$1}" = "yes" ; then
            GOT_BSD_HEADER=0
            GOT_BSD_LIB=0
            AC_CHECK_HEADERS(regex.h, [
                                 GOT_BSD_HEADER=1
                                 break
                             ])
            AC_SEARCH_LIBS(re_comp, bsd ucb regexp regex re, [GOT_BSD_LIB=1])
            if test $GOT_BSD_HEADER -eq 1 -a $GOT_BSD_LIB -eq 1 ; then
                AC_DEFINE([LIBAST_REGEXP_SUPPORT_BSD], [1], [Build LibAST with BSD-style regexp support.])
                LIBAST_REGEXP_SUPPORT="regexp-bsd"
                $1="bsd"
            else
                $1="yes"
            fi
        fi
        if test "${$1}" = "yes" ; then
            LIBAST_REGEXP_SUPPORT=""
            $1="no"
        fi
    else
        LIBAST_REGEXP_SUPPORT=""
        $1="no"
    fi
    AC_SUBST(LIBAST_REGEXP_SUPPORT)
])

dnl#
dnl# LibAST macro for X11 support
dnl#
AC_DEFUN([AST_X11_SUPPORT], [
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
    LIBAST_X11_SUPPORT=""
    if test "x$no_x" != "xyes"; then
        AC_CHECK_LIB(X11, XOpenDisplay, [
                         LIBAST_X11_SUPPORT="X11"
                         GRLIBS="-lX11"
                         AC_DEFINE([LIBAST_X11_SUPPORT], [1], [Define for X11 support.])
                     ])
    fi
    AC_SUBST(LIBAST_X11_SUPPORT)
])

dnl#
dnl# LibAST macro for Imlib2 support
dnl#
AC_DEFUN([AST_IMLIB2_SUPPORT], [
    AC_ARG_WITH(imlib,
    [  --with-imlib[=DIR]      compile with Imlib2 support (default)],
    [
        if test "$withval" != "no"; then 
            if test "$withval" != "yes"; then
                CPPFLAGS="$CPPFLAGS -I${withval}/include"
                LDFLAGS="$LDFLAGS -L${withval}/lib"
            fi
            USE_IMLIB=1
        else
            USE_IMLIB=0
        fi
    ], [
        USE_IMLIB=1
    ])
    LIBAST_IMLIB2_SUPPORT=""
    if test $USE_IMLIB -eq 1 ; then
        AC_CHECK_PROG(IMLIB2_CONFIG, imlib2-config, imlib2-config)
            if test "x$IMLIB2_CONFIG" != "x"; then
                GRLIBS="`$IMLIB2_CONFIG --libs`"
                CFLAGS="$CFLAGS `$IMLIB2_CONFIG --cflags`"
                AC_DEFINE([LIBAST_IMLIB2_SUPPORT], [1], [Define for Imlib2 support.])
                LIBAST_IMLIB2_SUPPORT="Imlib2"
            else
                AC_CHECK_LIB(m, pow, LIBS="-lm $LIBS")
                AC_CHECK_LIB(dl, dlopen, LIBS="-ldl $LIBS")
                AC_CHECK_LIB(freetype, FT_Init_FreeType, GRLIBS="-lfreetype $GRLIBS", , $GRLIBS)
                AC_CHECK_LIB(Imlib2, imlib_create_image, [
                                GRLIBS="-lImlib2 $GRLIBS"
                                AC_DEFINE([LIBAST_IMLIB2_SUPPORT], [1], [Define for Imlib2 support.])
                                LIBAST_IMLIB2_SUPPORT="Imlib2"
                     ], [
                         AC_WARN(*** Imlib2 support has been disabled because Imlib2 ***)
                         AC_WARN(*** was not found or could not be linked.           ***)
                     ], $GRLIBS)
            fi
    fi
    AC_SUBST(LIBAST_IMLIB2_SUPPORT)
])

dnl#
dnl# LibAST macro for MMX support
dnl#
AC_DEFUN([AST_MMX_SUPPORT], [
    AC_MSG_CHECKING(for MMX support)
    HAVE_MMX=""
    AC_ARG_ENABLE(mmx, [  --enable-mmx            enable MMX assembly routines], [
                     test x$enableval = xyes && HAVE_MMX="yes"
                  ], [
                     if test x$build_os = xlinux-gnu; then
                         grep mmx /proc/cpuinfo >/dev/null 2>&1 && HAVE_MMX="yes"
                     fi
                  ])
    LIBAST_MMX_SUPPORT=""
    if test -n "$HAVE_MMX"; then
        AC_MSG_RESULT(yes)
        AC_DEFINE([LIBAST_MMX_SUPPORT], [1], [Define for MMX support.])
        LIBAST_MMX_SUPPORT="MMX"
    else
        AC_MSG_RESULT(no)
    fi
    AC_SUBST(LIBAST_MMX_SUPPORT)
])

dnl#
dnl# LibAST macros for standard checks
dnl#
AC_DEFUN([AST_STD_CHECKS], [
    AC_PROG_CPP

    dnl# These must be run after AC_PROG_CC but before any other macros that use
    dnl# the C compiler
    AC_AIX
    AC_ISC_POSIX
    AC_MINIX

    dnl# At least make the attempt to support CygWin32
    AC_CYGWIN
    AC_ARG_PROGRAM

    AM_PROG_LIBTOOL

    AC_GCC_TRADITIONAL

    AC_PROG_INSTALL

    ASFLAGS="$ASFLAGS -I../"
    AS=$CC
    AC_SUBST(ASFLAGS)
    AC_SUBST(AS)

    dnl# Check for host system type
    AC_CANONICAL_HOST

    dnl# Check the sanity of what we've done so far
    AM_SANITY_CHECK

    dnl# Most people don't want the developer-only clutter
    AM_MAINTAINER_MODE

    dnl# If it's there, what the hell?
    AM_WITH_DMALLOC
])
AC_DEFUN([AST_PROG_CHECKS], [
    AC_CHECK_PROG(SED, sed, sed, false)
    AC_CHECK_PROG(RM, rm, rm, true)
    AC_CHECK_PROG(CP, cp, cp, false)
    AC_CHECK_PROG(CHMOD, chmod, chmod, true)
    AC_CHECK_PROG(TAR, tar, tar, tar)
    AC_CHECK_PROG(MKDIR, mkdir, mkdir, false)
    AC_CHECK_PROG(CTAGS, ctags, ctags, true)
    AC_CHECK_PROG(AR, ar, ar, false)
    AC_CHECK_PROG(MV, mv, mv, true)
    AC_LN_S
])
AC_DEFUN([AST_VAR_CHECKS], [
    AC_CHECK_SIZEOF(char, 1)
    AC_CHECK_SIZEOF(short, 2)
    AC_CHECK_SIZEOF(int, 4)
    AC_CHECK_SIZEOF(long, 4)
    AC_CHECK_SIZEOF(long long, 8)
    AC_C_BIGENDIAN

    AST_SIZE_TYPE(8)
    AC_SUBST(INT_8_TYPE)
    AST_SIZE_TYPE(16)
    AC_SUBST(INT_16_TYPE)
    AST_SIZE_TYPE(32)
    AC_SUBST(INT_32_TYPE)
    AST_SIZE_TYPE(64)
    AC_SUBST(INT_64_TYPE)

    AC_C_CONST
    AC_C_INLINE
])
AC_DEFUN([AST_HEADER_CHECKS], [
    AC_HEADER_SYS_WAIT
    AC_CHECK_HEADERS(fcntl.h termios.h sys/ioctl.h sys/select.h sys/time.h \
                     sys/sockio.h sys/byteorder.h malloc.h utmpx.h unistd.h \
                     bsd/signal.h stdarg.h errno.h)
    AC_HEADER_TIME
])
AC_DEFUN([AST_FUNC_CHECKS], [
    AC_TYPE_SIGNAL
    AC_CHECK_FUNCS(memmove putenv strsep memmem usleep snprintf vsnprintf \
                   strcasestr strcasechr strcasepbrk strrev strnlen)
    AC_SEARCH_LIBS(hstrerror, resolv)
    dps_snprintf_oflow()
    dps_vsnprintf_oflow()
    dps_symlink_open_bug()
    dps_rlimit_nproc()
    dps_rlimit_memlock()
])
AC_DEFUN([AST_TYPE_CHECKS], [
    AC_TYPE_MODE_T
    AC_CHECK_TYPE(off_t, long)
    AC_TYPE_PID_T
    AC_TYPE_UID_T
])

dnl#
dnl# LibAST argument macros
dnl#    - arg 1 is the name of the env var to use
dnl#
AC_DEFUN([AST_ARG_DEBUG], [
    AC_MSG_CHECKING(for debugging level)
    AC_ARG_WITH(debugging, [  --with-debugging[=num]  compile in debugging support.  num >= 0], [
                    if test "$withval" = "yes"; then
                        withval=4
                    fi
                    if test "$withval" != "no"; then 
                        AC_MSG_RESULT($withval)
                        AC_DEFINE_UNQUOTED([$1], $withval, [Specify level of debugging to compile in.])
                        $1=$withval
                    else
                        AC_MSG_RESULT(no, disabling all debugging support)
                        AC_DEFINE_UNQUOTED([$1], [0], [Specify level of debugging to compile in.])
                        $1=0
                    fi
                ], [
                    AC_MSG_RESULT(4)
                    AC_DEFINE_UNQUOTED([$1], [4], [Specify level of debugging to compile in.])
                    $1=4
    ])
])
AC_DEFUN([AST_ARG_REGEXP], [
    AC_ARG_WITH(regexp,
    [  --with-regexp[=TYPE]  specify the type of regular expression support (bsd, posix, pcre)],
    [$1=$withval], [$1=yes])
    AST_REGEXP_SUPPORT($1)
    AC_MSG_CHECKING(for regular expression support)
    AC_MSG_RESULT(${$1})
])
AC_DEFUN([AST_ARG_BACKQUOTE_EXEC], [
    AC_MSG_CHECKING(if backquote execution support should be enabled)
    AC_ARG_WITH(backquote-exec,
        [  --without-backquote-exec   disables the execution of commands from inside config files],
        [
            if test "$withval" = "no"; then
                AC_MSG_RESULT(no)
                $1=no
            else
                AC_MSG_RESULT(yes)
                AC_DEFINE($1, [1], [Define for backquote execution.])
                $1=yes
            fi
        ], [
            AC_MSG_RESULT(yes)
            AC_DEFINE($1, [1], [Define for backquote execution.])
            $1=yes
        ])
])

dnl#
dnl# LibAST macro for flag post-processing
dnl#
AC_DEFUN([AST_FLAGS], [
    CPPFLAGS=`eval eval eval eval eval echo "-I$includedir -I$prefix/include $CPPFLAGS"`
    CPPFLAGS=`echo $CPPFLAGS | tr ' ' '\n' | uniq | grep -v NONE | tr '\n' ' '`
    CFLAGS=${CFLAGS--O}
    LDFLAGS=`eval eval eval eval eval echo "-L$libdir -L$prefix/lib ${LDFLAGS--O}"`
    LDFLAGS=`echo $LDFLAGS | tr ' ' '\n' | uniq | grep -v NONE | tr '\n' ' '`
    LIBS="$GRLIBS $X_PRE_LIBS $LIBS $X_EXTRA_LIBS"
])

dnl#
dnl# LibAST macro for final status report
dnl#
AC_DEFUN([AST_STATUS], [
    echo ""
    echo "$PACKAGE $VERSION"
    echo "Configuration:"
    echo "--------------"
    echo ""
    echo "  Source code location:    $srcdir"
    echo "  Host System Type:        $host"
    echo "  Preprocessor:            $CC $CPPFLAGS"
    echo "  Compiler:                $CC $CFLAGS"
    echo "  Linker:                  $CC $LDFLAGS $LIBS"
    echo "  Install path:            $prefix"
    echo ""
    echo "Now type 'make' to build $PACKAGE $VERSION."
    echo ""
])

dnl###########################################################################

dnl#
dnl# acl.m4 -- Written by Duncan Simpson <dps@io.stargate.co.uk>
dnl# Posted to BUGTRAQ on 17 June 1999
dnl# Used by encouragement. :-)
dnl#

dnl Check snprintf for overrun potential
AC_DEFUN([dps_snprintf_oflow], [
    AC_MSG_CHECKING(whether snprintf ignores n)
    AC_CACHE_VAL(dps_cv_snprintf_bug, [
        AC_TRY_RUN(
            changequote(<<, >>)dnl
<<#include <stdio.h>

#ifndef HAVE_SNPRINTF
#include "src/snprintf.c"
#endif /* HAVE_SNPRINTF */

int main(void)
{
char ovbuf[7];
int i;
for (i=0; i<7; i++) ovbuf[i]='x';
snprintf(ovbuf, 4,"foo%s", "bar");
if (ovbuf[5]!='x') exit(1);
snprintf(ovbuf, 4,"foo%d", 666);
if (ovbuf[5]!='x') exit(1);
exit(0);
} >>
            changequote([, ])
        , dps_cv_snprintf_bug=0, dps_cv_snprintf_bug=1, dps_cv_snprintf_bug=2)
    ])
    if test $dps_cv_snprintf_bug -eq 0; then
        AC_MSG_RESULT([no, snprintf is ok])
    elif test $dps_cv_snprint_bug -eq 1; then
        AC_MSG_RESULT([yes, snprintf is broken])
        AC_DEFINE([HAVE_SNPRINTF_BUG], [1], [Defined if libc snprintf is buggy.])
    else
        AC_MSG_RESULT([unknown, assuming yes])
        AC_DEFINE([HAVE_SNPRINTF_BUG], [1], [Defined if libc snprintf is buggy.])
    fi
])

dnl Check vsnprintf for overrun potential
AC_DEFUN([dps_vsnprintf_oflow], [
    AC_MSG_CHECKING(whether vsnprintf ignores n)
    AC_CACHE_VAL(dps_cv_vsnprintf_bug, [
        AC_TRY_RUN(
            changequote(<<, >>)dnl
<<#include <stdio.h>
#include <stdarg.h>

#ifndef HAVE_VSNPRINTF
#include "src/snprintf.c"
#endif /* HAVE_VSNPRINTF */

int prnt(char *s, const char *fmt, ...)
{
  va_list argp;
  va_start(argp, fmt);
  vsnprintf(s, 4, fmt, argp);
  va_end(argp);
}

int main(void)
{
  char ovbuf[8] = "xxxxxxx";
  int i;
  prnt(ovbuf, "foo%s", "bar");
  if (ovbuf[5]!='x') {fprintf(stderr, "buffer:  %s\n", ovbuf); exit(1);}
  prnt(ovbuf, "foo%d", 666);
  if (ovbuf[5]!='x') {fprintf(stderr, "buffer:  %s\n", ovbuf); exit(1);}
  exit(0);
} >>
            changequote([, ])
        , dps_cv_vsnprintf_bug=0, dps_cv_vsnprintf_bug=1, dps_cv_vsnprintf_bug=2)
    ])

    if test $dps_cv_vsnprintf_bug -eq 0; then
        AC_MSG_RESULT([no, vsnprintf is ok])
    elif test $dps_cv_vsnprintf_bug -eq 1; then
        AC_MSG_RESULT([yes, vsnprintf is broken])
        AC_DEFINE([HAVE_VSNPRINTF_BUG], [1], [Defined if libc vsnprintf is buggy.])
    else
        AC_MSG_RESULT([unknown, assuming yes])
        AC_DEFINE([HAVE_VSNPRINTF_BUG], [1], [Defined if libc vsnprintf is buggy.])
    fi
])

dnl open and symlink interaction bug test
AC_DEFUN([dps_symlink_open_bug], [
    AC_MSG_CHECKING(security of interaction between symlink and open)
    AC_CACHE_VAL(dps_cv_symlink_open_bug, [
        mkdir conftest.d
        AC_TRY_RUN(
            changequote(<<, >>)dnl
<<#include <stdio.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_ERRNO_H
#include <errno.h>
#else
extern int errno;
#endif

int main(void)
{
  int fd;
  if (chdir("conftest.d")!=0)
    exit(1);
  if (symlink("foo","bar")!=0)
    exit(1);
  if ((fd=open("bar", O_CREAT | O_EXCL | O_WRONLY, 0700))==0)
  {
        write(fd, "If the symlink was to .rhosts you would be unhappy", 50);
	close(fd);
	exit(1);
  }
  if (errno!=EEXIST)
    exit(1);
  exit(0);
} >>
            changequote([, ])
        ,
        dps_cv_symlink_open_bug=0,
        [
            if test -r conftest.d/foo; then
                dps_cv_symlink_open_bug=2
            else
                dps_cv_symlink_open_bug=1
            fi
        ],
        dps_cv_symlink_open_buf=3)
        rm -rf conftest.d
    ])
    case "$dps_cv_symlink_open_bug" in
        0) AC_MSG_RESULT(secure) ;;
        1) AC_MSG_RESULT(errno wrong but ok)
           AC_DEFINE([HAVE_SYMLINK_OPEN_ERRNO_BUG], [1], [Defined if symlink open() errno is wrong but safe.]) ;;
        2) AC_MSG_RESULT(insecure)
           AC_DEFINE([HAVE_SYMLINK_OPEN_SECURITY_HOLE], [1], [Defined if symlink open() is a security risk.])
           AC_DEFINE([HAVE_SYMLINK_OPEN_ERRNO_BUG], [1], [Defined if symlink open() is buggy.]) ;;
        3) AC_MSG_RESULT(assuming insecure)
           AC_DEFINE([HAVE_SYMLINK_OPEN_SECURITY_HOLE], [1], [Defined if symlink open() is a security risk.])
           AC_DEFINE([HAVE_SYMLINK_OPEN_ERRNO_BUG], [1], [Defined if symlink open() is buggy.]) ;;
        *) AC_MSG_RESULT($dps_cv_symlink_open_bug)
           AC_MSG_ERROR(Impossible value of dps_cv_symlink_open_bug) ;;
    esac
])

dnl Check to RLIMIT_NPROC resource limit
AC_DEFUN([dps_rlimit_nproc], [
    AC_MSG_CHECKING(for working RLIMIT_NPROC resource limit)
    AC_CACHE_VAL(dps_cv_rlimit_nproc, [
        AC_TRY_RUN(
            changequote(<<, >>)dnl
<<
#ifndef HAVE_STDLIB_H
#include <stdlib.h>
#endif /* HAVE_STDLIB_H */
#ifndef HAVE_SIGNAL_H
#include <signal.h>
#endif /* HAVE_SIGNAL_H */
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#ifdef HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif /* HAVE_SYS_RESOURCE_H */

int main(void)
{
#ifdef RLIMIT_NPROC
    static const struct rlimit pid_lim={RLIMIT_NPROC, 1};
    pid_t f;

    signal(SIGCHLD, SIG_IGN);
    setrlimit(RLIMIT_NPROC, (struct rlimit *) &pid_lim);
    if ((f=fork())==0)
	exit(0);
    if (f==-1)
	exit(0); /* The fork() failed (the right thing) */
#endif
   exit(1);
} >>
            changequote([, ])
        , dps_cv_rlimit_nproc=0, dps_cv_rlimit_nproc=1, dps_cv_rlimit_nproc=2)
    ])
    if test $dps_cv_rlimit_nproc -eq 0; then
        AC_MSG_RESULT([yes])
        AC_DEFINE([HAVE_RLIMIT_NPROC], [1], [Defined if the RLIMIT_NPROC resource limit works.])
    elif test $dps_cv_rlimit_nproc -eq 1; then
        AC_MSG_RESULT([no])
    else
        AC_MSG_RESULT([unknown, assuming none])
    fi
])

dnl Check to RLIMIT_MEMLOCK resource limit
AC_DEFUN([dps_rlimit_memlock], [
    AC_MSG_CHECKING(for working RLIMIT_MEMLOCK resource limit)
    AC_CACHE_VAL(dps_cv_rlimit_memlock, [
        AC_TRY_RUN(
            changequote(<<, >>)dnl
<<
#ifndef HAVE_STDLIB_H
#include <stdlib.h>
#endif /* HAVE_STDLIB_H */
#ifndef HAVE_SIGNAL_H
#include <signal.h>
#endif /* HAVE_SIGNAL_H */
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#ifdef HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif /* HAVE_SYS_RESOURCE_H */
#ifdef HAVE_SYS_MMAN
#include <sys/mman.h>
#endif /* HAVE_SYS_MMAN */
#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif /* HAVE_ERRNO_H */

int main(void)
{
#ifdef RLIMIT_MEMLOCK
    static const struct rlimit mlock_lim={RLIMIT_MEMLOCK, 0};
    void *memory;

    if (setrlimit(RLIMIT_MEMLOCK, (struct rlimit *) &mlock_lim)!=-1)
	exit(0);
#endif
exit(1);
} >>
            changequote([, ])
        , dps_cv_rlimit_memlock=0, dps_cv_rlimit_memlock=1, dps_cv_rlimit_memlock=2)
    ])
    if test $dps_cv_rlimit_memlock -eq 0; then
        AC_MSG_RESULT([yes])
        AC_DEFINE([HAVE_RLIMIT_MEMLOCK], [1], [Defined if the RLIMIT_MEMLOCK resource limit works.])
    elif test $dps_cv_rlimit_memlock -eq 1; then
        AC_MSG_RESULT([no])
    else
        AC_MSG_RESULT([unknown, assuming none])
    fi
])
