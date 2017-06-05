/************************************************************************/
/*                                                                      */
/*                         Applied Type System                          */
/*                                                                      */
/*                              Hongwei Xi                              */
/*                                                                      */
/************************************************************************/

/*
** ATS - Unleashing the Potential of Types!
**
** Copyright (C) 2002-2010 Hongwei Xi.
**
** ATS is  free software;  you can redistribute it and/or modify it under
** the  terms of the  GNU General Public License as published by the Free
** Software Foundation; either version 2.1, or (at your option) any later
** version.
** 
** ATS is distributed in the hope that it will be useful, but WITHOUT ANY
** WARRANTY; without  even  the  implied  warranty  of MERCHANTABILITY or
** FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public License
** for more details.
** 
** You  should  have  received  a  copy of the GNU General Public License
** along  with  ATS;  see the  file COPYING.  If not, please write to the
** Free Software Foundation,  51 Franklin Street, Fifth Floor, Boston, MA
** 02110-1301, USA.
*/

/* ****** ****** */

/* author: Hongwei Xi (hwxi AT cs DOT bu DOT edu) */

/* ****** ****** */

#ifndef ATS_LIBC_SIGNAL_CATS
#define ATS_LIBC_SIGNAL_CATS

/* ****** ****** */

#include <signal.h>

/* ****** ****** */

typedef ats_int_type signum_t ;
typedef void (*sighandler_t)(signum_t) ;

/* ****** ****** */

typedef struct sigaction ats_sigaction_type ;

/* ****** ****** */

#define atslib_sigaction sigaction
#define atslib_sigaction_null(signum, act) \
  sigaction(signum, act, (ats_sigaction_type*)0)

/* ****** ****** */

#define atslib_sigemptyset sigemptyset
#define atslib_sigfillset sigfillset
#define atslib_sigaddset sigaddset
#define atslib_sigdelset sigdelset

/* ****** ****** */

#define atslib_pthread_sigmask pthread_sigmask
#define atslib_pthread_sigmask_null(how, newset) pthread_sigmask(how, newset, NULL)

#define atslib_sigprocmask sigprocmask
#define atslib_sigprocmask_null(how, newset) sigprocmask(how, newset, NULL)

/* ****** ****** */

#define atslib_signal signal
#define atslib_sigset sigset

#define atslib_sighold sighold
#define atslib_sigignore sigignore
#define atslib_sigrelse sigrelse

/* ****** ****** */

#define atslib_kill kill
#define atslib_killpg killpg
#define atslib_pthread_kill pthread_kill
#define atslib_raise raies

/* ****** ****** */

#define atslib_sigwait sigwait
#define atslib_sigsuspend sigsuspend
#define atslib_sigpause sigpause

/* ****** ****** */

#define atslib_sigpending sigpending
#define atslib_siginterrupt siginterrupt

/* ****** ****** */

#define atslib_psignal psignal

extern
char* strsignal (signum_t sgn) ; // declared in <string.h>
#define atslib_strsignal strsignal

/* ****** ****** */

#endif /* ATS_LIBC_SIGNAL_CATS */
