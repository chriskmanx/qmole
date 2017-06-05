/*============================================================================
  Kitware Information Macro Library
  Copyright 2010-2011 Kitware, Inc.
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions
  are met:

  * Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.

  * Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.

  * Neither the name of Kitware, Inc. nor the names of its contributors
    may be used to endorse or promote products derived from this
    software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
  HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
============================================================================*/
#ifndef cmIML_ABI_H
#define cmIML_ABI_H
/*
This header defines macros with information about the C ABI.
Only information that can be determined using the preprocessor at
compilation time is available.  No try-compile results may be added
here.  Instead we memorize results on platforms of interest.

An includer may optionally define the following macros to suppress errors:

  cmIML_ABI_NO_VERIFY          = skip verification declarations
  cmIML_ABI_NO_ERROR_CHAR_SIGN = signedness of 'char' may be unknown
  cmIML_ABI_NO_ERROR_LONG_LONG = existence of 'long long' may be unknown
  cmIML_ABI_NO_ERROR_ENDIAN    = byte order of CPU may be unknown

An includer may test the following macros after inclusion:

  cmIML_ABI_SIZEOF_DATA_PTR   = sizeof(void*)
  cmIML_ABI_SIZEOF_CODE_PTR   = sizeof(void(*)(void))
  cmIML_ABI_SIZEOF_FLOAT      = sizeof(float)
  cmIML_ABI_SIZEOF_DOUBLE     = sizeof(double)
  cmIML_ABI_SIZEOF_CHAR       = sizeof(char)
  cmIML_ABI_SIZEOF_SHORT      = sizeof(short)
  cmIML_ABI_SIZEOF_INT        = sizeof(int)
  cmIML_ABI_SIZEOF_LONG       = sizeof(long)

  cmIML_ABI_SIZEOF_LONG_LONG  = sizeof(long long) or 0 if not a type
    Undefined if existence is unknown and error suppression macro
    cmIML_ABI_NO_ERROR_LONG_LONG was defined.

  cmIML_ABI_SIZEOF___INT64    = 8 if '__int64' exists or 0 if not
    Undefined if existence is unknown.

  cmIML_ABI___INT64_IS_LONG   = 1 if '__int64' is 'long' (same type)
    Undefined otherwise.
  cmIML_ABI___INT64_IS_LONG_LONG = 1 if '__int64' is 'long long' (same type)
    Undefined otherwise.
  cmIML_ABI___INT64_IS_UNIQUE = 1 if '__int64' is a distinct type
    Undefined otherwise.

  cmIML_ABI_CHAR_IS_UNSIGNED  = 1 if 'char' is unsigned, else undefined
  cmIML_ABI_CHAR_IS_SIGNED    = 1 if 'char' is signed, else undefined
    One of these is defined unless signedness of 'char' is unknown and
    error suppression macro cmIML_ABI_NO_ERROR_CHAR_SIGN was defined.

  cmIML_ABI_ENDIAN_ID_BIG    = id for big-endian (always defined)
  cmIML_ABI_ENDIAN_ID_LITTLE = id for little-endian (always defined)
  cmIML_ABI_ENDIAN_ID        = id of byte order of target CPU
    Defined to cmIML_ABI_ENDIAN_ID_BIG or cmIML_ABI_ENDIAN_ID_LITTLE
    unless byte order is unknown and error suppression macro
    cmIML_ABI_NO_ERROR_ENDIAN was defined.

We verify most results using dummy "extern" declarations that are
invalid if the macros are wrong.  Verification is disabled if
suppression macro cmIML_ABI_NO_VERIFY was defined.
*/

/*--------------------------------------------------------------------------*/
#if !defined(cmIML_ABI_SIZEOF_DATA_PTR)
# if defined(__SIZEOF_POINTER__)
#  define cmIML_ABI_SIZEOF_DATA_PTR __SIZEOF_POINTER__
# elif defined(_SIZE_PTR)
#  define cmIML_ABI_SIZEOF_DATA_PTR (_SIZE_PTR >> 3)
# elif defined(_LP64) || defined(__LP64__)
#  define cmIML_ABI_SIZEOF_DATA_PTR 8
# elif defined(_ILP32)
#  define cmIML_ABI_SIZEOF_DATA_PTR 4
# elif defined(__64BIT__) /* IBM XL */
#  define cmIML_ABI_SIZEOF_DATA_PTR 8
# elif defined(_M_X64)
#  define cmIML_ABI_SIZEOF_DATA_PTR 8
# elif defined(__ia64)
#  define cmIML_ABI_SIZEOF_DATA_PTR 8
# elif defined(__sparcv9)
#  define cmIML_ABI_SIZEOF_DATA_PTR 8
# elif defined(__x86_64) || defined(__x86_64__)
#  define cmIML_ABI_SIZEOF_DATA_PTR 8
# elif defined(__amd64) || defined(__amd64__)
#  define cmIML_ABI_SIZEOF_DATA_PTR 8
# elif defined(__i386) || defined(__i386__)
#  define cmIML_ABI_SIZEOF_DATA_PTR 4
# endif
#endif
#if !defined(cmIML_ABI_SIZEOF_DATA_PTR)
# define cmIML_ABI_SIZEOF_DATA_PTR 4
#endif
#if !defined(cmIML_ABI_SIZEOF_CODE_PTR)
# define cmIML_ABI_SIZEOF_CODE_PTR cmIML_ABI_SIZEOF_DATA_PTR
#endif

/*--------------------------------------------------------------------------*/
#if !defined(cmIML_ABI_SIZEOF_CHAR)
# define cmIML_ABI_SIZEOF_CHAR 1
#endif

#if !defined(cmIML_ABI_CHAR_IS_UNSIGNED) && !defined(cmIML_ABI_CHAR_IS_SIGNED)
# if defined(__CHAR_UNSIGNED__) /* GNU, some IBM XL, others?  */
#  define cmIML_ABI_CHAR_IS_UNSIGNED 1
# elif defined(_CHAR_UNSIGNED) /* Intel, IBM XL, MSVC, Borland, others?  */
#  define cmIML_ABI_CHAR_IS_UNSIGNED 1
# elif defined(_CHAR_SIGNED) /* IBM XL, others? */
#  define cmIML_ABI_CHAR_IS_SIGNED 1
# elif defined(__CHAR_SIGNED__) /* IBM XL, Watcom, others? */
#  define cmIML_ABI_CHAR_IS_SIGNED 1
# elif defined(__SIGNED_CHARS__) /* EDG, Intel, SGI MIPSpro */
#  define cmIML_ABI_CHAR_IS_SIGNED 1
# elif defined(_CHAR_IS_SIGNED) /* Some SunPro, others? */
#  define cmIML_ABI_CHAR_IS_SIGNED 1
# elif defined(_CHAR_IS_UNSIGNED) /* SunPro, others? */
#  define cmIML_ABI_CHAR_IS_UNSIGNED 1
# elif defined(__GNUC__) /* GNU default */
#  define cmIML_ABI_CHAR_IS_SIGNED 1
# elif defined(__SUNPRO_C) || defined(__SUNPRO_CC) /* SunPro default */
#  define cmIML_ABI_CHAR_IS_SIGNED 1
# elif defined(__HP_cc) || defined(__HP_aCC) /* HP default (unless +uc) */
#  define cmIML_ABI_CHAR_IS_SIGNED 1
# elif defined(_SGI_COMPILER_VERSION) /* SGI MIPSpro default */
#  define cmIML_ABI_CHAR_IS_UNSIGNED 1
# elif defined(__PGIC__) /* PGI default */
#  define cmIML_ABI_CHAR_IS_SIGNED 1
# elif defined(_MSC_VER) /* MSVC default */
#  define cmIML_ABI_CHAR_IS_SIGNED 1
# elif defined(__WATCOMC__) /* Watcom default */
#  define cmIML_ABI_CHAR_IS_UNSIGNED 1
# elif defined(__BORLANDC__) /* Borland default */
#  define cmIML_ABI_CHAR_IS_SIGNED 1
# elif defined(__hpux) /* Old HP: no __HP_cc/__HP_aCC/__GNUC__ above */
#  define cmIML_ABI_CHAR_IS_SIGNED 1 /* (unless +uc) */
# endif
#endif
#if !defined(cmIML_ABI_CHAR_IS_UNSIGNED) && !defined(cmIML_ABI_CHAR_IS_SIGNED) \
 && !defined(cmIML_ABI_NO_ERROR_CHAR_SIGN)
# error "Signedness of 'char' unknown."
#endif

/*--------------------------------------------------------------------------*/
#if !defined(cmIML_ABI_SIZEOF_SHORT)
# if defined(__SIZEOF_SHORT__)
#  define cmIML_ABI_SIZEOF_SHORT __SIZEOF_SHORT__
# endif
#endif
#if !defined(cmIML_ABI_SIZEOF_SHORT)
# define cmIML_ABI_SIZEOF_SHORT 2
#endif

/*--------------------------------------------------------------------------*/
#if !defined(cmIML_ABI_SIZEOF_INT)
# if defined(__SIZEOF_INT__)
#  define cmIML_ABI_SIZEOF_INT __SIZEOF_INT__
# elif defined(_SIZE_INT)
#  define cmIML_ABI_SIZEOF_INT (_SIZE_INT >> 3)
# endif
#endif
#if !defined(cmIML_ABI_SIZEOF_INT)
# define cmIML_ABI_SIZEOF_INT 4
#endif

/*--------------------------------------------------------------------------*/
#if !defined(cmIML_ABI_SIZEOF_LONG)
# if defined(__SIZEOF_LONG__)
#  define cmIML_ABI_SIZEOF_LONG __SIZEOF_LONG__
# elif defined(_SIZE_LONG)
#  define cmIML_ABI_SIZEOF_LONG (_SIZE_LONG >> 3)
# elif defined(__LONG_MAX__)
#  if __LONG_MAX__ == 0x7fffffff
#   define cmIML_ABI_SIZEOF_LONG 4
#  elif __LONG_MAX__>>32 == 0x7fffffff
#   define cmIML_ABI_SIZEOF_LONG 8
#  endif
# elif defined(_MSC_VER) /* MSVC and Intel on Windows */
#  define cmIML_ABI_SIZEOF_LONG 4
# endif
#endif
#if !defined(cmIML_ABI_SIZEOF_LONG)
# define cmIML_ABI_SIZEOF_LONG cmIML_ABI_SIZEOF_DATA_PTR
#endif

/*--------------------------------------------------------------------------*/
#if !defined(cmIML_ABI_SIZEOF_LONG_LONG)
# if defined(__SIZEOF_LONG_LONG__)
#  define cmIML_ABI_SIZEOF_LONG_LONG __SIZEOF_LONG_LONG__
# elif defined(__LONG_LONG_MAX__)
#  if __LONG_LONG_MAX__ == 0x7fffffff
#   define cmIML_ABI_SIZEOF_LONG_LONG 4
#  elif __LONG_LONG_MAX__>>32 == 0x7fffffff
#   define cmIML_ABI_SIZEOF_LONG_LONG 8
#  endif
# endif
#endif
#if !defined(cmIML_ABI_SIZEOF_LONG_LONG)
# if defined(_LONGLONG) /* SGI, some GNU, perhaps others.  */ \
  && !defined(_MSC_VER)
#  define cmIML_ABI_SIZEOF_LONG_LONG 8
# elif defined(_LONG_LONG) /* IBM XL, perhaps others.  */
#  define cmIML_ABI_SIZEOF_LONG_LONG 8
# elif defined(__NO_LONG_LONG) /* EDG */
#  define cmIML_ABI_SIZEOF_LONG_LONG 0
# elif defined(__cplusplus) && __cplusplus > 199711L /* C++0x */
#  define cmIML_ABI_SIZEOF_LONG_LONG 8
# elif defined(__STDC_VERSION__) && __STDC_VERSION__ >= 199901L /* C99 */
#  define cmIML_ABI_SIZEOF_LONG_LONG 8
# elif defined(__SUNPRO_C) || defined(__SUNPRO_CC) /* SunPro */
#  define cmIML_ABI_SIZEOF_LONG_LONG 8
# elif defined(__HP_cc) || defined(__HP_aCC) /* HP */
#  define cmIML_ABI_SIZEOF_LONG_LONG 8
# elif defined(__PGIC__) /* PGI */
#  define cmIML_ABI_SIZEOF_LONG_LONG 8
# elif defined(__WATCOMC__) /* Watcom */
#  define cmIML_ABI_SIZEOF_LONG_LONG 8
# elif defined(__INTEL_COMPILER) /* Intel */
#  define cmIML_ABI_SIZEOF_LONG_LONG 8
# elif defined(__BORLANDC__) /* Borland */
#  if __BORLANDC__ >= 0x0560
#   define cmIML_ABI_SIZEOF_LONG_LONG 8
#  else
#   define cmIML_ABI_SIZEOF_LONG_LONG 0
#  endif
# elif defined(_MSC_VER) /* Microsoft */
#  if _MSC_VER >= 1310
#   define cmIML_ABI_SIZEOF_LONG_LONG 8
#  else
#   define cmIML_ABI_SIZEOF_LONG_LONG 0
#  endif
# elif defined(__hpux) && !defined(__GNUC__) /* Old HP: no __HP_cc/__HP_aCC above */
#  define cmIML_ABI_SIZEOF_LONG_LONG 8
# endif
#endif
#if !defined(cmIML_ABI_SIZEOF_LONG_LONG) && !defined(cmIML_ABI_NO_ERROR_LONG_LONG)
# error "Existence of 'long long' unknown."
#endif

/*--------------------------------------------------------------------------*/
#if !defined(cmIML_ABI_SIZEOF___INT64)
# if defined(__INTEL_COMPILER)
#  define cmIML_ABI_SIZEOF___INT64 8
# elif defined(_MSC_VER)
#  define cmIML_ABI_SIZEOF___INT64 8
# elif defined(__BORLANDC__)
#  define cmIML_ABI_SIZEOF___INT64 8
# else
#  define cmIML_ABI_SIZEOF___INT64 0
# endif
#endif

#if defined(cmIML_ABI_SIZEOF___INT64) && cmIML_ABI_SIZEOF___INT64 > 0
# if cmIML_ABI_SIZEOF_LONG == 8
#  define cmIML_ABI___INT64_IS_LONG 1
# elif defined(cmIML_ABI_SIZEOF_LONG_LONG) && cmIML_ABI_SIZEOF_LONG_LONG == 8
#  define cmIML_ABI___INT64_IS_LONG_LONG 1
# else
#  define cmIML_ABI___INT64_IS_UNIQUE 1
# endif
#endif

/*--------------------------------------------------------------------------*/
#if !defined(cmIML_ABI_SIZEOF_FLOAT)
# if defined(__SIZEOF_FLOAT__)
#  define cmIML_ABI_SIZEOF_FLOAT __SIZEOF_FLOAT__
# endif
#endif
#if !defined(cmIML_ABI_SIZEOF_FLOAT)
# define cmIML_ABI_SIZEOF_FLOAT 4
#endif

/*--------------------------------------------------------------------------*/
#if !defined(cmIML_ABI_SIZEOF_DOUBLE)
# if defined(__SIZEOF_DOUBLE__)
#  define cmIML_ABI_SIZEOF_DOUBLE __SIZEOF_DOUBLE__
# endif
#endif
#if !defined(cmIML_ABI_SIZEOF_DOUBLE)
# define cmIML_ABI_SIZEOF_DOUBLE 8
#endif

/*--------------------------------------------------------------------------*/
/* Identify possible endian cases.  The macro cmIML_ABI_ENDIAN_ID will be
   defined to one of these, or undefined if unknown.  */
#if !defined(cmIML_ABI_ENDIAN_ID_BIG)
# define cmIML_ABI_ENDIAN_ID_BIG    4321
#endif
#if !defined(cmIML_ABI_ENDIAN_ID_LITTLE)
# define cmIML_ABI_ENDIAN_ID_LITTLE 1234
#endif
#if cmIML_ABI_ENDIAN_ID_BIG == cmIML_ABI_ENDIAN_ID_LITTLE
# error "cmIML_ABI_ENDIAN_ID_BIG == cmIML_ABI_ENDIAN_ID_LITTLE"
#endif

#if defined(cmIML_ABI_ENDIAN_ID) /* Skip #elif cases if already defined.  */

/* Use dedicated symbols if the compiler defines them.  Do this first
   because some architectures allow runtime byte order selection by
   the operating system (values for such architectures below are
   guesses for compilers that do not define a dedicated symbol).
   Ensure that only one is defined in case the platform or a header
   defines both as possible values for some third symbol.  */
#elif defined(_BIG_ENDIAN) && !defined(_LITTLE_ENDIAN)
# define cmIML_ABI_ENDIAN_ID cmIML_ABI_ENDIAN_ID_BIG
#elif defined(_LITTLE_ENDIAN) && !defined(_BIG_ENDIAN)
# define cmIML_ABI_ENDIAN_ID cmIML_ABI_ENDIAN_ID_LITTLE
#elif defined(__BIG_ENDIAN__) && !defined(__LITTLE_ENDIAN__)
# define cmIML_ABI_ENDIAN_ID cmIML_ABI_ENDIAN_ID_BIG
#elif defined(__LITTLE_ENDIAN__) && !defined(__BIG_ENDIAN__)
# define cmIML_ABI_ENDIAN_ID cmIML_ABI_ENDIAN_ID_LITTLE

/* Alpha */
#elif defined(__alpha) || defined(__alpha__) || defined(_M_ALPHA)
# define cmIML_ABI_ENDIAN_ID cmIML_ABI_ENDIAN_ID_LITTLE

/* Arm */
#elif defined(__arm__)
# if !defined(__ARMEB__)
#  define cmIML_ABI_ENDIAN_ID cmIML_ABI_ENDIAN_ID_LITTLE
# else
#  define cmIML_ABI_ENDIAN_ID cmIML_ABI_ENDIAN_ID_BIG
# endif

/* Intel x86 */
#elif defined(__i386) || defined(__i386__) || defined(_M_IX86)
# define cmIML_ABI_ENDIAN_ID cmIML_ABI_ENDIAN_ID_LITTLE
#elif defined(_X86_) || defined(__THW_INTEL__) || defined(__I86__)
# define cmIML_ABI_ENDIAN_ID cmIML_ABI_ENDIAN_ID_LITTLE
#elif defined(__MWERKS__) && defined(__INTEL__)
# define cmIML_ABI_ENDIAN_ID cmIML_ABI_ENDIAN_ID_LITTLE

/* Intel x86-64 */
#elif defined(__x86_64) || defined(__x86_64__) || defined(_M_X64)
# define cmIML_ABI_ENDIAN_ID cmIML_ABI_ENDIAN_ID_LITTLE
#elif defined(__amd64) || defined(__amd64__)
# define cmIML_ABI_ENDIAN_ID cmIML_ABI_ENDIAN_ID_LITTLE

/* Intel Architecture-64 (Itanium) */
#elif defined(__ia64) || defined(__ia64__)
# define cmIML_ABI_ENDIAN_ID cmIML_ABI_ENDIAN_ID_LITTLE
#elif defined(_IA64) || defined(__IA64__) || defined(_M_IA64)
# define cmIML_ABI_ENDIAN_ID cmIML_ABI_ENDIAN_ID_LITTLE

/* PowerPC */
#elif defined(__powerpc) || defined(__powerpc__)
# define cmIML_ABI_ENDIAN_ID cmIML_ABI_ENDIAN_ID_BIG
#elif defined(__ppc) || defined(__ppc__) || defined(__POWERPC__)
# define cmIML_ABI_ENDIAN_ID cmIML_ABI_ENDIAN_ID_BIG

/* SPARC */
#elif defined(__sparc) || defined(__sparc__)
# define cmIML_ABI_ENDIAN_ID cmIML_ABI_ENDIAN_ID_BIG

/* HP/PA RISC */
#elif defined(__hppa) || defined(__hppa__)
# define cmIML_ABI_ENDIAN_ID cmIML_ABI_ENDIAN_ID_BIG

/* Motorola 68k */
#elif defined(__m68k__) || defined(M68000)
# define cmIML_ABI_ENDIAN_ID cmIML_ABI_ENDIAN_ID_BIG

/* MIPSel (MIPS little endian) */
#elif defined(__MIPSEL__) || defined(__MIPSEL) || defined(_MIPSEL)
# define cmIML_ABI_ENDIAN_ID cmIML_ABI_ENDIAN_ID_LITTLE

/* MIPSeb (MIPS big endian) */
#elif defined(__MIPSEB__) || defined(__MIPSEB) || defined(_MIPSEB)
# define cmIML_ABI_ENDIAN_ID cmIML_ABI_ENDIAN_ID_BIG

/* MIPS (fallback, big endian) */
#elif defined(__mips) || defined(__mips__) || defined(__MIPS__)
# define cmIML_ABI_ENDIAN_ID cmIML_ABI_ENDIAN_ID_BIG

/* RS/6000 */
#elif defined(__THW_RS600) || defined(_IBMR2) || defined(_POWER)
# define cmIML_ABI_ENDIAN_ID cmIML_ABI_ENDIAN_ID_BIG
#elif defined(_ARCH_PWR) || defined(_ARCH_PWR2)
# define cmIML_ABI_ENDIAN_ID cmIML_ABI_ENDIAN_ID_BIG

/* System/370 */
#elif defined(__370__) || defined(__THW_370__)
# define cmIML_ABI_ENDIAN_ID cmIML_ABI_ENDIAN_ID_BIG

/* System/390 */
#elif defined(__s390__) || defined(__s390x__)
# define cmIML_ABI_ENDIAN_ID cmIML_ABI_ENDIAN_ID_BIG

/* z/Architecture */
#elif defined(__SYSC_ZARCH__)
# define cmIML_ABI_ENDIAN_ID cmIML_ABI_ENDIAN_ID_BIG

/* Unknown CPU */
#elif !defined(cmIML_ABI_NO_ERROR_ENDIAN)
# error "Byte order of target CPU unknown."
#endif

/*--------------------------------------------------------------------------*/
#if !defined(cmIML_ABI_NO_VERIFY)
#define cmIML_ABI__VERIFY(n, x, y) extern int (*n)[x]; extern int (*n)[y]
#define cmIML_ABI__VERIFY2(n, x, y) extern int (*n)(x*); extern int (*n)(y*)
#if defined(__cplusplus)
# define cmIML_ABI__VERIFY3(n, x, y) extern int* n(x*); extern char* n(y*)
#else
# define cmIML_ABI__VERIFY3(n, x, y) extern int* n(x*) /* TODO: possible? */
#endif
#define cmIML_ABI__VERIFY_BOOL(m, b) cmIML_ABI__VERIFY(m##__VERIFY__, 2, (b)?2:3)
#define cmIML_ABI__VERIFY_SIZE(m, t) cmIML_ABI__VERIFY(m##__VERIFY__, m, sizeof(t))
#define cmIML_ABI__VERIFY_SAME(m, x, y) cmIML_ABI__VERIFY2(m##__VERIFY__, x, y)
#define cmIML_ABI__VERIFY_DIFF(m, x, y) cmIML_ABI__VERIFY3(m##__VERIFY__, x, y)

cmIML_ABI__VERIFY_SIZE(cmIML_ABI_SIZEOF_DATA_PTR, int*);
cmIML_ABI__VERIFY_SIZE(cmIML_ABI_SIZEOF_CODE_PTR, int(*)(int));
cmIML_ABI__VERIFY_SIZE(cmIML_ABI_SIZEOF_CHAR, char);
cmIML_ABI__VERIFY_SIZE(cmIML_ABI_SIZEOF_SHORT, short);
cmIML_ABI__VERIFY_SIZE(cmIML_ABI_SIZEOF_INT, int);
cmIML_ABI__VERIFY_SIZE(cmIML_ABI_SIZEOF_LONG, long);
#if defined(cmIML_ABI_SIZEOF_LONG_LONG) && cmIML_ABI_SIZEOF_LONG_LONG > 0
cmIML_ABI__VERIFY_SIZE(cmIML_ABI_SIZEOF_LONG_LONG, long long);
#endif
#if defined(cmIML_ABI_SIZEOF___INT64) && cmIML_ABI_SIZEOF___INT64 > 0
cmIML_ABI__VERIFY_SIZE(cmIML_ABI_SIZEOF___INT64, __int64);
#endif
cmIML_ABI__VERIFY_SIZE(cmIML_ABI_SIZEOF_FLOAT, float);
cmIML_ABI__VERIFY_SIZE(cmIML_ABI_SIZEOF_DOUBLE, double);

#if defined(cmIML_ABI___INT64_IS_LONG)
cmIML_ABI__VERIFY_SAME(cmIML_ABI___INT64_IS_LONG, __int64, long);
#elif defined(cmIML_ABI___INT64_IS_LONG_LONG)
cmIML_ABI__VERIFY_SAME(cmIML_ABI___INT64_IS_LONG_LONG, __int64, long long);
#elif defined(cmIML_ABI_SIZEOF___INT64) && cmIML_ABI_SIZEOF___INT64 > 0
cmIML_ABI__VERIFY_DIFF(cmIML_ABI___INT64_NOT_LONG, __int64, long);
# if defined(cmIML_ABI_SIZEOF_LONG_LONG) && cmIML_ABI_SIZEOF_LONG_LONG > 0
cmIML_ABI__VERIFY_DIFF(cmIML_ABI___INT64_NOT_LONG_LONG, __int64, long long);
# endif
#endif

#if defined(cmIML_ABI_CHAR_IS_UNSIGNED)
cmIML_ABI__VERIFY_BOOL(cmIML_ABI_CHAR_IS_UNSIGNED, (char)0x80 > 0);
#elif defined(cmIML_ABI_CHAR_IS_SIGNED)
cmIML_ABI__VERIFY_BOOL(cmIML_ABI_CHAR_IS_SIGNED,   (char)0x80 < 0);
#endif

#undef cmIML_ABI__VERIFY_DIFF
#undef cmIML_ABI__VERIFY_SAME
#undef cmIML_ABI__VERIFY_SIZE
#undef cmIML_ABI__VERIFY_BOOL
#undef cmIML_ABI__VERIFY3
#undef cmIML_ABI__VERIFY2
#undef cmIML_ABI__VERIFY

#endif

#endif
