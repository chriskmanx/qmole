/*
 * Copyright (C) 1997-2009, Michael Jennings
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to
 * deal in the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies of the Software, its documentation and marketing & publicity
 * materials, and acknowledgment shall be given in the documentation, materials
 * and software packages that this Software was used.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

#ifndef ETERM_UTMP_H_
#define ETERM_UTMP_H_

#include <X11/Xfuncproto.h>
#include <X11/Intrinsic.h>	/* Xlib, Xutil, Xresource, Xfuncproto */

#ifdef UTMP_SUPPORT

# include <stdio.h>
# include <string.h>
/* For some systems (HP-UX in particular), sys/types.h must be included
   before utmp*.h -- mej */
# include <sys/types.h>
# include <sys/stat.h>
/* Unsupported/broken utmpx.h on HP-UX, AIX, and glibc 2.1 */
# if defined(_HPUX_SOURCE) || defined(_AIX) || ((__GLIBC__ >= 2) && (__GLIBC_MINOR__ >= 1))
#   undef HAVE_UTMPX_H
# endif
# ifdef HAVE_UTMPX_H
#  include <utmpx.h>
#  define USE_SYSV_UTMP
# else
#  include <utmp.h>
#  ifdef HAVE_SETUTENT
#   define USE_SYSV_UTMP
#  endif
# endif
# ifdef TIME_WITH_SYS_TIME
#  include <sys/time.h>
#  include <time.h>
# else
#  ifdef HAVE_SYS_TIME_H
#   include <sys/time.h>
#  else
#   include <time.h>
#  endif
# endif
# ifdef HAVE_UNISTD_H
#  include <unistd.h>
# endif
# include <pwd.h>
# include <errno.h>
# ifdef HAVE_FCNTL_H
#  include <fcntl.h>
# endif
# ifdef HAVE_LASTLOG_H
#  include <lastlog.h>
# endif
# if defined(__FreeBSD__) || defined(__NetBSD__) || defined(__bsdi__) || defined(__DragonFly__)
#  include <ttyent.h>
#  define NEW_BSD_UTMP
# endif

# ifdef HAVE_LIBUTEMPTER
#  include <utempter.h>
#  define add_utmp_entry(p, h, f)  addToUtmp(p, h, f)
#  define remove_utmp_entry()      removeFromUtmp()
# endif

/************ Macros and Definitions ************/
# ifndef UTMP_FILENAME
#   ifdef UTMP_FILE
#     define UTMP_FILENAME UTMP_FILE
#   elif defined(_PATH_UTMP)
#     define UTMP_FILENAME _PATH_UTMP
#   else
#     define UTMP_FILENAME "/etc/utmp"
#   endif
# endif

# ifndef LASTLOG_FILENAME
#  ifdef _PATH_LASTLOG
#   define LASTLOG_FILENAME _PATH_LASTLOG
#  else
#   define LASTLOG_FILENAME "/usr/adm/lastlog"	/* only on BSD systems */
#  endif
# endif

# ifndef WTMP_FILENAME
#   ifdef WTMP_FILE
#     define WTMP_FILENAME WTMP_FILE
#   elif defined(_PATH_WTMP)
#     define WTMP_FILENAME _PATH_WTMP
#   elif defined(SYSV)
#     define WTMP_FILENAME "/etc/wtmp"
#   else
#     define WTMP_FILENAME "/usr/adm/wtmp"
#   endif
# endif

# ifndef TTYTAB_FILENAME
#   ifdef TTYTAB
#     define TTYTAB_FILENAME TTYTAB_FILENAME
#   else
#     define TTYTAB_FILENAME "/etc/ttytab"
#   endif
# endif

# ifndef USER_PROCESS
#   define USER_PROCESS 7
# endif
# ifndef DEAD_PROCESS
#   define DEAD_PROCESS 8
# endif

/************ Function Prototypes ************/
_XFUNCPROTOBEGIN

# ifndef HAVE_LIBUTEMPTER
extern void add_utmp_entry(const char *, const char *, int);
extern void remove_utmp_entry(void);
# endif

_XFUNCPROTOEND

#else /* UTMP_SUPPORT */
# define add_utmp_entry(p, h, f)  NOP
# define remove_utmp_entry()      NOP
#endif

#endif	/* ETERM_UTMP_H_ */
