/* DO NOT EDIT! GENERATED AUTOMATICALLY! */
/* -*- buffer-read-only: t -*- vi: set ro: */
/* DO NOT EDIT! GENERATED AUTOMATICALLY! */
/* A GNU-like <dirent.h>.
   Copyright (C) 2006-2010 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

#ifndef _GL_DIRENT_H

#if __GNUC__ >= 3
#pragma GCC system_header
#endif

/* The include_next requires a split double-inclusion guard.  */
#include_next <dirent.h>

#ifndef _GL_DIRENT_H
#define _GL_DIRENT_H

/* Get ino_t.  Needed on some systems, including glibc 2.8.  */
#include <sys/types.h>

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


/* Declare overridden functions.  */

#if 0
# if !(defined __cplusplus && defined GNULIB_NAMESPACE)
#  define closedir rpl_closedir
# endif
_GL_FUNCDECL_RPL (closedir, int, (DIR *) _GL_ARG_NONNULL ((1)));
_GL_CXXALIAS_RPL (closedir, int, (DIR *));
#else
_GL_CXXALIAS_SYS (closedir, int, (DIR *));
#endif
_GL_CXXALIASWARN (closedir);

#if 1
# if !1 && !defined dirfd
/* Return the file descriptor associated with the given directory stream,
   or -1 if none exists.  */
_GL_EXTERN_C int dirfd (DIR *dir) _GL_ARG_NONNULL ((1));
# endif
#elif defined GNULIB_POSIXCHECK
# undef dirfd
# if HAVE_RAW_DECL_DIRFD
_GL_WARN_ON_USE (dirfd, "dirfd is unportable - "
                 "use gnulib module dirfd for portability");
# endif
#endif

#if 1
/* Open a directory stream visiting the given directory file
   descriptor.  Return NULL and set errno if fd is not visiting a
   directory.  On success, this function consumes fd (it will be
   implicitly closed either by this function or by a subsequent
   closedir).  */
# if 0
#  if !(defined __cplusplus && defined GNULIB_NAMESPACE)
#   undef fdopendir
#   define fdopendir rpl_fdopendir
#  endif
_GL_FUNCDECL_RPL (fdopendir, DIR *, (int fd));
_GL_CXXALIAS_RPL (fdopendir, DIR *, (int fd));
# else
#  if !0
_GL_FUNCDECL_SYS (fdopendir, DIR *, (int fd));
#  endif
_GL_CXXALIAS_SYS (fdopendir, DIR *, (int fd));
# endif
_GL_CXXALIASWARN (fdopendir);
#elif defined GNULIB_POSIXCHECK
# undef fdopendir
# if HAVE_RAW_DECL_FDOPENDIR
_GL_WARN_ON_USE (fdopendir, "fdopendir is unportable - "
                 "use gnulib module fdopendir for portability");
# endif
#endif

#if 0
# if !(defined __cplusplus && defined GNULIB_NAMESPACE)
#  define opendir rpl_opendir
# endif
_GL_FUNCDECL_RPL (opendir, DIR *, (const char *) _GL_ARG_NONNULL ((1)));
_GL_CXXALIAS_RPL (opendir, DIR *, (const char *));
#else
_GL_CXXALIAS_SYS (opendir, DIR *, (const char *));
#endif
_GL_CXXALIASWARN (opendir);

#if 0
/* Scan the directory DIR, calling FILTER on each directory entry.
   Entries for which FILTER returns nonzero are individually malloc'd,
   sorted using qsort with CMP, and collected in a malloc'd array in
   *NAMELIST.  Returns the number of entries selected, or -1 on error.  */
# if !1
_GL_FUNCDECL_SYS (scandir, int,
                  (const char *dir, struct dirent ***namelist,
                   int (*filter) (const struct dirent *),
                   int (*cmp) (const struct dirent **, const struct dirent **))
                  _GL_ARG_NONNULL ((1, 2, 4)));
# endif
/* Need to cast, because on glibc systems, the fourth parameter is
                        int (*cmp) (const void *, const void *).  */
_GL_CXXALIAS_SYS_CAST (scandir, int,
                       (const char *dir, struct dirent ***namelist,
                        int (*filter) (const struct dirent *),
                        int (*cmp) (const struct dirent **, const struct dirent **)));
_GL_CXXALIASWARN (scandir);
#elif defined GNULIB_POSIXCHECK
# undef scandir
# if HAVE_RAW_DECL_SCANDIR
_GL_WARN_ON_USE (scandir, "scandir is unportable - "
                 "use gnulib module scandir for portability");
# endif
#endif

#if 0
/* Compare two 'struct dirent' entries alphabetically.  */
# if !1
_GL_FUNCDECL_SYS (alphasort, int,
                  (const struct dirent **, const struct dirent **)
                  _GL_ARG_NONNULL ((1, 2)));
# endif
/* Need to cast, because on glibc systems, the parameters are
                       (const void *, const void *).  */
_GL_CXXALIAS_SYS_CAST (alphasort, int,
                       (const struct dirent **, const struct dirent **));
_GL_CXXALIASWARN (alphasort);
#elif defined GNULIB_POSIXCHECK
# undef alphasort
# if HAVE_RAW_DECL_ALPHASORT
_GL_WARN_ON_USE (alphasort, "alphasort is unportable - "
                 "use gnulib module alphasort for portability");
# endif
#endif


#endif /* _GL_DIRENT_H */
#endif /* _GL_DIRENT_H */
