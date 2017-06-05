dnl check for libspamc required includes

AC_DEFUN([AC_SPAMASSASSIN],
[dnl

AC_CHECK_HEADERS(sys/time.h syslog.h unistd.h errno.h sys/errno.h)
AC_CHECK_HEADERS(time.h sysexits.h sys/socket.h netdb.h netinet/in.h)

AC_CACHE_CHECK([for SHUT_RD],
       spamassassin_cv_has_shutrd, [
                AC_TRY_COMPILE([#include <sys/types.h>
#include <sys/socket.h>],
                        [printf ("%d", SHUT_RD); return 0;],
                                        [spamassassin_cv_has_shutrd=yes],
                                        [spamassassin_cv_has_shutrd=no]),
       ])
if test $spamassassin_cv_has_shutrd = yes ; then
  AC_DEFINE(HAVE_SHUT_RD, 1, HAVE_SHUT_RD)
fi

dnl ----------------------------------------------------------------------

AC_CHECK_FUNCS(socket strdup strtod strtol snprintf shutdown)

dnl ----------------------------------------------------------------------

AC_CACHE_CHECK([for h_errno],
        spamassassin_cv_has_herrno, [
                AC_TRY_COMPILE([#include <netdb.h>],
                        [printf ("%d", h_errno); return 0;],
                                        [spamassassin_cv_has_herrno=yes],
                                        [spamassassin_cv_has_herrno=no]),
        ])
if test $spamassassin_cv_has_herrno = yes ; then
  AC_DEFINE(HAVE_H_ERRNO, 1, HAVE_H_ERRNO)
fi

dnl ----------------------------------------------------------------------

dnl ----------------------------------------------------------------------

AC_CACHE_CHECK([for in_addr_t],
        spamassassin_cv_has_inaddrt, [
                AC_TRY_COMPILE([#include <sys/types.h>
#include <netinet/in.h>],
                        [in_addr_t foo; return 0;],
                                        [spamassassin_cv_has_inaddrt=yes],
                                        [spamassassin_cv_has_inaddrt=no]),
        ])
if test $spamassassin_cv_has_inaddrt = no ; then
  AC_CHECK_TYPE(in_addr_t, unsigned long)
fi

dnl ----------------------------------------------------------------------

AC_CACHE_CHECK([for INADDR_NONE],
        spamassassin_cv_has_haveinaddrnone, [
                AC_TRY_COMPILE([#include <sys/types.h>
#include <netinet/in.h>],
                        [in_addr_t foo = INADDR_NONE; return 0;],
                                        [spamassassin_cv_has_haveinaddrnone=yes],
                                        [spamassassin_cv_has_haveinaddrnone=no]),
        ])
if test $spamassassin_cv_has_haveinaddrnone = yes ; then
  AC_DEFINE(HAVE_INADDR_NONE, 1, HAVE_INADDR_NONE)
fi

dnl ----------------------------------------------------------------------

AC_CACHE_CHECK([for EX__MAX],
        spamassassin_cv_has_haveexmax, [
                AC_TRY_COMPILE([#ifdef HAVE_SYSEXITS_H
#include <sysexits.h>
#endif
#include <errno.h>],
                        [int foo = EX__MAX; return 0;],
                                        [spamassassin_cv_has_haveexmax=yes],
                                        [spamassassin_cv_has_haveexmax=no]),
        ])
if test $spamassassin_cv_has_haveexmax = yes ; then
  AC_DEFINE(HAVE_EX__MAX, 1, HAVE_EX__MAX)
fi

])
