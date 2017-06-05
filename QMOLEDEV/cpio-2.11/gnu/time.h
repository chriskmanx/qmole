/* DO NOT EDIT! GENERATED AUTOMATICALLY! */
/* -*- buffer-read-only: t -*- vi: set ro: */
/* DO NOT EDIT! GENERATED AUTOMATICALLY! */
/* A more-standard <time.h>.

   Copyright (C) 2007-2010 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.  */

#if __GNUC__ >= 3
#pragma GCC system_header
#endif

/* Don't get in the way of glibc when it includes time.h merely to
   declare a few standard symbols, rather than to declare all the
   symbols.  Also, Solaris 8 <time.h> eventually includes itself
   recursively; if that is happening, just include the system <time.h>
   without adding our own declarations.  */
#if (defined __need_time_t || defined __need_clock_t \
     || defined __need_timespec \
     || defined _GL_TIME_H)

# include_next <time.h>

#else

# define _GL_TIME_H

# include_next <time.h>

/* NetBSD 5.0 mis-defines NULL.  */
#include <stddef.h>

/* The definitions of _GL_FUNCDECL_RPL etc. are copied here.  */
#ifndef _GL_CXXDEFS_H
#define _GL_CXXDEFS_H

/* The three most frequent use cases of these macros are:

   * For providing a substitute for a function that is missing on some
     platforms, but is declared and works fine on the platforms on which
     it exists:

       #if @GNULIB_FOO@
       # if !@HAVE_FOO@
       _GL_FUNCDECL_SYS (foo, ...);
       # endif
       _GL_CXXALIAS_SYS (foo, ...);
       _GL_CXXALIASWARN (foo);
       #elif defined GNULIB_POSIXCHECK
       ...
       #endif

   * For providing a replacement for a function that exists on all platforms,
     but is broken/insufficient and needs to be replaced on some platforms:

       #if @GNULIB_FOO@
       # if @REPLACE_FOO@
       #  if !(defined __cplusplus && defined GNULIB_NAMESPACE)
       #   undef foo
       #   define foo rpl_foo
       #  endif
       _GL_FUNCDECL_RPL (foo, ...);
       _GL_CXXALIAS_RPL (foo, ...);
       # else
       _GL_CXXALIAS_SYS (foo, ...);
       # endif
       _GL_CXXALIASWARN (foo);
       #elif defined GNULIB_POSIXCHECK
       ...
       #endif

   * For providing a replacement for a function that exists on some platforms
     but is broken/insufficient and needs to be replaced on some of them and
     is additionally either missing or undeclared on some other platforms:

       #if @GNULIB_FOO@
       # if @REPLACE_FOO@
       #  if !(defined __cplusplus && defined GNULIB_NAMESPACE)
       #   undef foo
       #   define foo rpl_foo
       #  endif
       _GL_FUNCDECL_RPL (foo, ...);
       _GL_CXXALIAS_RPL (foo, ...);
       # else
       #  if !@HAVE_FOO@   or   if !@HAVE_DECL_FOO@
       _GL_FUNCDECL_SYS (foo, ...);
       #  endif
       _GL_CXXALIAS_SYS (foo, ...);
       # endif
       _GL_CXXALIASWARN (foo);
       #elif defined GNULIB_POSIXCHECK
       ...
       #endif
*/

/* _GL_EXTERN_C declaration;
   performs the declaration with C linkage.  */
#if defined __cplusplus
# define _GL_EXTERN_C extern "C"
#else
# define _GL_EXTERN_C extern
#endif

/* _GL_FUNCDECL_RPL (func, rettype, parameters_and_attributes);
   declares a replacement function, named rpl_func, with the given prototype,
   consisting of return type, parameters, and attributes.
   Example:
     _GL_FUNCDECL_RPL (open, int, (const char *filename, int flags, ...)
                                  _GL_ARG_NONNULL ((1)));
 */
#define _GL_FUNCDECL_RPL(func,rettype,parameters_and_attributes) \
  _GL_FUNCDECL_RPL_1 (rpl_##func, rettype, parameters_and_attributes)
#define _GL_FUNCDECL_RPL_1(rpl_func,rettype,parameters_and_attributes) \
  _GL_EXTERN_C rettype rpl_func parameters_and_attributes

/* _GL_FUNCDECL_SYS (func, rettype, parameters_and_attributes);
   declares the system function, named func, with the given prototype,
   consisting of return type, parameters, and attributes.
   Example:
     _GL_FUNCDECL_SYS (open, int, (const char *filename, int flags, ...)
                                  _GL_ARG_NONNULL ((1)));
 */
#define _GL_FUNCDECL_SYS(func,rettype,parameters_and_attributes) \
  _GL_EXTERN_C rettype func parameters_and_attributes

/* _GL_CXXALIAS_RPL (func, rettype, parameters);
   declares a C++ alias called GNULIB_NAMESPACE::func
   that redirects to rpl_func, if GNULIB_NAMESPACE is defined.
   Example:
     _GL_CXXALIAS_RPL (open, int, (const char *filename, int flags, ...));
 */
#define _GL_CXXALIAS_RPL(func,rettype,parameters) \
  _GL_CXXALIAS_RPL_1 (func, rpl_##func, rettype, parameters)
#if defined __cplusplus && defined GNULIB_NAMESPACE
# define _GL_CXXALIAS_RPL_1(func,rpl_func,rettype,parameters) \
    namespace GNULIB_NAMESPACE                                \
    {                                                         \
      rettype (*const func) parameters = ::rpl_func;          \
    }                                                         \
    _GL_EXTERN_C int _gl_cxxalias_dummy
#else
# define _GL_CXXALIAS_RPL_1(func,rpl_func,rettype,parameters) \
    _GL_EXTERN_C int _gl_cxxalias_dummy
#endif

/* _GL_CXXALIAS_SYS (func, rettype, parameters);
   declares a C++ alias called GNULIB_NAMESPACE::func
   that redirects to the system provided function func, if GNULIB_NAMESPACE
   is defined.
   Example:
     _GL_CXXALIAS_SYS (open, int, (const char *filename, int flags, ...));
 */
#if defined __cplusplus && defined GNULIB_NAMESPACE
  /* If we were to write
       rettype (*const func) parameters = ::func;
     like above in _GL_CXXALIAS_RPL_1, the compiler could optimize calls
     better (remove an indirection through a 'static' pointer variable),
     but then the _GL_CXXALIASWARN macro below would cause a warning not only
     for uses of ::func but also for uses of GNULIB_NAMESPACE::func.  */
# define _GL_CXXALIAS_SYS(func,rettype,parameters) \
    namespace GNULIB_NAMESPACE                     \
    {                                              \
      static rettype (*func) parameters = ::func;  \
    }                                              \
    _GL_EXTERN_C int _gl_cxxalias_dummy
#else
# define _GL_CXXALIAS_SYS(func,rettype,parameters) \
    _GL_EXTERN_C int _gl_cxxalias_dummy
#endif

/* _GL_CXXALIAS_SYS_CAST (func, rettype, parameters);
   is like  _GL_CXXALIAS_SYS (func, rettype, parameters);
   except that the C function func may have a slightly different declaration.
   A cast is used to silence the "invalid conversion" error that would
   otherwise occur.  */
#if defined __cplusplus && defined GNULIB_NAMESPACE
# define _GL_CXXALIAS_SYS_CAST(func,rettype,parameters) \
    namespace GNULIB_NAMESPACE                          \
    {                                                   \
      static rettype (*func) parameters =               \
        reinterpret_cast<rettype(*)parameters>(::func); \
    }                                                   \
    _GL_EXTERN_C int _gl_cxxalias_dummy
#else
# define _GL_CXXALIAS_SYS_CAST(func,rettype,parameters) \
    _GL_EXTERN_C int _gl_cxxalias_dummy
#endif

/* _GL_CXXALIAS_SYS_CAST2 (func, rettype, parameters, rettype2, parameters2);
   is like  _GL_CXXALIAS_SYS (func, rettype, parameters);
   except that the C function is picked among a set of overloaded functions,
   namely the one with rettype2 and parameters2.  Two consecutive casts
   are used to silence the "cannot find a match" and "invalid conversion"
   errors that would otherwise occur.  */
#if defined __cplusplus && defined GNULIB_NAMESPACE
# define _GL_CXXALIAS_SYS_CAST2(func,rettype,parameters,rettype2,parameters2) \
    namespace GNULIB_NAMESPACE                                                \
    {                                                                         \
      static rettype (*func) parameters =                                     \
        reinterpret_cast<rettype(*)parameters>(                               \
          reinterpret_cast<rettype2(*)parameters2>(::func));                  \
    }                                                                         \
    _GL_EXTERN_C int _gl_cxxalias_dummy
#else
# define _GL_CXXALIAS_SYS_CAST(func,rettype,parameters) \
    _GL_EXTERN_C int _gl_cxxalias_dummy
#endif

/* _GL_CXXALIASWARN (func);
   causes a warning to be emitted when ::func is used but not when
   GNULIB_NAMESPACE::func is used.  */
#if defined __cplusplus && defined GNULIB_NAMESPACE
# define _GL_CXXALIASWARN(func) \
   _GL_CXXALIASWARN1 (func, GNULIB_NAMESPACE)
# define _GL_CXXALIASWARN1(func,namespace) \
   _GL_CXXALIASWARN2 (func, namespace)
# define _GL_CXXALIASWARN2(func,namespace) \
   _GL_WARN_ON_USE (func, \
                    "The symbol ::" #func " refers to the system function. " \
                    "Use " #namespace "::" #func " instead.")
#else
# define _GL_CXXALIASWARN(func) \
    _GL_EXTERN_C int _gl_cxxalias_dummy
#endif

#endif /* _GL_CXXDEFS_H */

/* The definition of _GL_ARG_NONNULL is copied here.  */
/* _GL_ARG_NONNULL((n,...,m)) tells the compiler and static analyzer tools
   that the values passed as arguments n, ..., m must be non-NULL pointers.
   n = 1 stands for the first argument, n = 2 for the second argument etc.  */
#ifndef _GL_ARG_NONNULL
# if (__GNUC__ == 3 && __GNUC_MINOR__ >= 3) || __GNUC__ > 3
#  define _GL_ARG_NONNULL(params) __attribute__ ((__nonnull__ params))
# else
#  define _GL_ARG_NONNULL(params)
# endif
#endif

/* The definition of _GL_WARN_ON_USE is copied here.  */
#ifndef _GL_WARN_ON_USE

# if 4 < __GNUC__ || (__GNUC__ == 4 && 3 <= __GNUC_MINOR__)
/* A compiler attribute is available in gcc versions 4.3.0 and later.  */
#  define _GL_WARN_ON_USE(function, message) \
extern __typeof__ (function) function __attribute__ ((__warning__ (message)))

# else /* Unsupported.  */
#  define _GL_WARN_ON_USE(function, message) \
extern int _gl_warn_on_use
# endif
#endif

# ifdef __cplusplus
extern "C" {
# endif

/* Some systems don't define struct timespec (e.g., AIX 4.1, Ultrix 4.3).
   Or they define it with the wrong member names or define it in <sys/time.h>
   (e.g., FreeBSD circa 1997).  */
# if ! 1
#  if 0
#   include <sys/time.h>
#  else
#   undef timespec
#   define timespec rpl_timespec
struct timespec
{
  time_t tv_sec;
  long int tv_nsec;
};
#  endif
# endif

# ifdef __cplusplus
}
# endif

/* Sleep for at least RQTP seconds unless interrupted,  If interrupted,
   return -1 and store the remaining time into RMTP.  See
   <http://www.opengroup.org/susv3xsh/nanosleep.html>.  */
# if 0
#  if GNULIB_PORTCHECK
#   if !(defined __cplusplus && defined GNULIB_NAMESPACE)
#    define nanosleep rpl_nanosleep
#   endif
_GL_FUNCDECL_RPL (nanosleep, int,
                  (struct timespec const *__rqtp, struct timespec *__rmtp)
                  _GL_ARG_NONNULL ((1)));
_GL_CXXALIAS_RPL (nanosleep, int,
                  (struct timespec const *__rqtp, struct timespec *__rmtp));
#  else
_GL_CXXALIAS_SYS (nanosleep, int,
                  (struct timespec const *__rqtp, struct timespec *__rmtp));
#  endif
_GL_CXXALIASWARN (nanosleep);
# endif

/* Return the 'time_t' representation of TP and normalize TP.  */
# if 1
#  if 1
#   if !(defined __cplusplus && defined GNULIB_NAMESPACE)
#    define mktime rpl_mktime
#   endif
_GL_FUNCDECL_RPL (mktime, time_t, (struct tm *__tp) _GL_ARG_NONNULL ((1)));
_GL_CXXALIAS_RPL (mktime, time_t, (struct tm *__tp));
#  else
_GL_CXXALIAS_SYS (mktime, time_t, (struct tm *__tp));
#  endif
_GL_CXXALIASWARN (mktime);
# endif

/* Convert TIMER to RESULT, assuming local time and UTC respectively.  See
   <http://www.opengroup.org/susv3xsh/localtime_r.html> and
   <http://www.opengroup.org/susv3xsh/gmtime_r.html>.  */
# if 0
#  if !(defined __cplusplus && defined GNULIB_NAMESPACE)
#   undef localtime_r
#   define localtime_r rpl_localtime_r
#  endif
_GL_FUNCDECL_RPL (localtime_r, struct tm *, (time_t const *restrict __timer,
                                             struct tm *restrict __result)
                                            _GL_ARG_NONNULL ((1, 2)));
_GL_CXXALIAS_RPL (localtime_r, struct tm *, (time_t const *restrict __timer,
                                             struct tm *restrict __result));
# else
_GL_CXXALIAS_SYS (localtime_r, struct tm *, (time_t const *restrict __timer,
                                             struct tm *restrict __result));
# endif
_GL_CXXALIASWARN (localtime_r);
# if 0
#  if !(defined __cplusplus && defined GNULIB_NAMESPACE)
#   undef gmtime_r
#   define gmtime_r rpl_gmtime_r
#  endif
_GL_FUNCDECL_RPL (gmtime_r, struct tm *, (time_t const *restrict __timer,
                                          struct tm *restrict __result)
                                         _GL_ARG_NONNULL ((1, 2)));
_GL_CXXALIAS_RPL (gmtime_r, struct tm *, (time_t const *restrict __timer,
                                          struct tm *restrict __result));
# else
_GL_CXXALIAS_SYS (gmtime_r, struct tm *, (time_t const *restrict __timer,
                                          struct tm *restrict __result));
# endif
_GL_CXXALIASWARN (gmtime_r);

/* Parse BUF as a time stamp, assuming FORMAT specifies its layout, and store
   the resulting broken-down time into TM.  See
   <http://www.opengroup.org/susv3xsh/strptime.html>.  */
# if 0
#  if GNULIB_PORTCHECK
#   if !(defined __cplusplus && defined GNULIB_NAMESPACE)
#    undef strptime
#    define strptime rpl_strptime
#   endif
_GL_FUNCDECL_RPL (strptime, char *, (char const *restrict __buf,
                                     char const *restrict __format,
                                     struct tm *restrict __tm)
                                    _GL_ARG_NONNULL ((1, 2, 3)));
_GL_CXXALIAS_RPL (strptime, char *, (char const *restrict __buf,
                                     char const *restrict __format,
                                     struct tm *restrict __tm));
#  else
_GL_CXXALIAS_SYS (strptime, char *, (char const *restrict __buf,
                                     char const *restrict __format,
                                     struct tm *restrict __tm));
#  endif
_GL_CXXALIASWARN (strptime);
# endif

/* Convert TM to a time_t value, assuming UTC.  */
# if 0
#  if GNULIB_PORTCHECK
#   if !(defined __cplusplus && defined GNULIB_NAMESPACE)
#    undef timegm
#    define timegm rpl_timegm
#   endif
_GL_FUNCDECL_RPL (timegm, time_t, (struct tm *__tm) _GL_ARG_NONNULL ((1)));
_GL_CXXALIAS_RPL (timegm, time_t, (struct tm *__tm));
#  else
_GL_CXXALIAS_SYS (timegm, time_t, (struct tm *__tm));
#  endif
_GL_CXXALIASWARN (timegm);
# endif

/* Encourage applications to avoid unsafe functions that can overrun
   buffers when given outlandish struct tm values.  Portable
   applications should use strftime (or even sprintf) instead.  */
# if GNULIB_PORTCHECK
#  undef asctime
#  define asctime eschew_asctime
#  undef asctime_r
#  define asctime_r eschew_asctime_r
#  undef ctime
#  define ctime eschew_ctime
#  undef ctime_r
#  define ctime_r eschew_ctime_r
# endif

#endif
