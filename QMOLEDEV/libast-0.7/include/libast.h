/*
 * Copyright (C) 1997-2004, Michael Jennings
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

/**
 * @file libast.h
 * Global LibAST header file.
 *
 * This file contains all general-purpose macros, function
 * declarations, etc. for LibAST.  It is also responsible for
 * including all required system headers and LibAST Object headers.
 *
 * @author Michael Jennings <mej@eterm.org>
 * @version $Revision: 1.61 $
 * @date $Date: 2005/07/16 01:39:24 $
 */

#ifndef _LIBAST_H_
#define _LIBAST_H_

#include <libast/sysdefs.h>

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <unistd.h>
#include <ctype.h>
#include <string.h>
#include <fcntl.h>
#include <dirent.h>
#include <errno.h>
#include <signal.h>
#include <limits.h>
#include <math.h>
#if TIME_WITH_SYS_TIME
# include <sys/time.h>
#endif
#if WITH_DMALLOC
# include <dmalloc.h>
#elif HAVE_MALLOC_H
# include <malloc.h>
#endif

#include <netdb.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <sys/un.h>
#include <arpa/inet.h>
#include <netinet/tcp.h>
/*#include <netinet/udp.h>*/

#if LIBAST_X11_SUPPORT
# include <X11/Xatom.h>
# include <X11/X.h>
# include <X11/Intrinsic.h>
# if LIBAST_IMLIB2_SUPPORT
#  include <Imlib2.h>
# endif
#endif

#ifdef __GNUC__
#  if __GNUC__ >= 4
#    undef  STRICT_ISO_C99
#    define STRICT_ISO_C99 1
#  endif
#else
#  define __attribute__(x)
#  define __extension__(x)
#  define __volatile__(x)
#endif

#if LIBAST_REGEXP_SUPPORT_PCRE
#  if HAVE_PCRE_H
#    include <pcre.h>
#  elif HAVE_PCRE_PCRE_H
#    include <pcre/pcre.h>
#  endif
#elif LIBAST_REGEXP_SUPPORT_POSIX || LIBAST_REGEXP_SUPPORT_BSD
#  if HAVE_REGEX_H
#    include <regex.h>
#  endif
#  if LIBAST_REGEXP_SUPPORT_BSD
extern char *re_comp();
extern int re_exec();
#  endif
#endif

#include <libast/types.h>
#include <libast/obj.h>

#include <libast/mbuff.h>
#include <libast/objpair.h>
#include <libast/regexp.h>
#include <libast/socket.h>
#include <libast/str.h>
#include <libast/tok.h>
#include <libast/url.h>

#include <libast/iterator_if.h>
#include <libast/list_if.h>
#include <libast/map_if.h>
#include <libast/vector_if.h>

#include <libast/array.h>
#include <libast/linked_list.h>
#include <libast/dlinked_list.h>

#include <libast/avl_tree.h>

/******************************* GENERIC GOOP *********************************/
/**
 * Mark a variable as used.
 *
 * This macro is used to explicitly mark a variable as "used."  It
 * intentionally generates no real code, but suppresses gcc warnings
 * about unused variables and/or parameters.  That way, the programmer
 * can explicitly acknowledge that certain variables/parameters are
 * intentionally unused, making the warnings more effective by
 * eliminating false positives.
 *
 * @param x Any variable or parameter name.
 */
#define USE_VAR(x)   (void) x

/**
 * @def MIN(a, b)
 * Return the lesser of @a a or @a b.
 *
 * This macro compares its two parameters, @a a and @a b, and returns
 * the lesser of the two (the minimum).  When building under gcc, a
 * GNU-specific extension is used which prevents expressions used as
 * parameters from being evaluated multiple times.
 *
 * @param a Any expression that evaluates to a value.
 * @param b Any expression that evaluates to a value.
 * @return The lesser of the two values.
 */
/**
 * @def MAX(a, b)
 * Return the greater of @a a or @a b.
 *
 * This macro compares its two parameters, @a a and @a b, and returns
 * the greater of the two (the maximum).  When building under gcc, a
 * GNU-specific extension is used which prevents expressions used as
 * parameters from being evaluated multiple times.
 *
 * @param a Any expression that evaluates to a value.
 * @param b Any expression that evaluates to a value.
 * @return The greater of the two values.
 */
/**
 * @def LOWER_BOUND(current, other)
 * Force a lower bound on a variable.
 *
 * This macro checks the value of its first parameter, @a current, and
 * makes sure it is greater than or equal to the value of @a other.
 * If @a current is less than @a other, @a current is assigned the
 * value of @a other.  In essence, this establishes a "lower bound" on
 * @a current equal to the value of @a other.
 *
 * @param current The variable to check.
 * @param other   The value by which @a current will be bound.
 * @return The new value of @a current.
 */
/**
 * @def UPPER_BOUND(current, other)
 * Force an upper bound on a variable.
 *
 * This macro checks the value of its first parameter, @a current, and
 * makes sure it is less than or equal to the value of @a other.  If
 * @a current is greater than @a other, @a current is assigned the
 * value of @a other.  In essence, this establishes an "upper bound"
 * on @a current equal to the value of @a other.
 *
 * @param current The variable to check.
 * @param other   The value by which @a current will be bound.
 * @return The new value of @a current.
 */
/**
 * @def BOUND(val, min, max)
 * Force a variable to be within a given range.
 *
 * This macro checks the value of its first parameter, @a val, and
 * makes sure it is between @a min and @a max, inclusive.  If @a val
 * is above this range, it is assigned the value of @a max.  Likewise,
 * if @a val is below this range, it is assigned the value of @a min.
 * In essence, this establishes both an "upper bound" and a "lower
 * bound" on @a val.
 *
 * @param val The variable to check.
 * @param min The lowest value @a val may have.
 * @param max The highest value @a val may have.
 * @return The new value of @a val.
 */
#ifdef MIN
# undef MIN
#endif
#ifdef MAX
# undef MAX
#endif
#ifdef __GNUC__
# define MIN(a,b)                       __extension__ ({__typeof__(a) aa = (a); __typeof__(b) bb = (b); (aa < bb) ? (aa) : (bb);})
# define MAX(a,b)                       __extension__ ({__typeof__(a) aa = (a); __typeof__(b) bb = (b); (aa > bb) ? (aa) : (bb);})
# define LOWER_BOUND(current, other)    __extension__ ({__typeof__(other) o = (other); ((current) < o) ? ((current) = o) : (current);})
# define UPPER_BOUND(current, other)    __extension__ ({__typeof__(other) o = (other); ((current) > o) ? ((current) = o) : (current);})
# define BOUND(val, min, max)           __extension__ ({__typeof__(min) m1 = (min); __typeof__(max) m2 = (max); ((val) < m1) ? ((val) = m1) : (((val) > m2) ? ((val) = m2) : (val));})
#else
# define MIN(a,b)                       (((a) < (b)) ? (a) : (b))
# define MAX(a,b)                       (((a) > (b)) ? (a) : (b))
# define LOWER_BOUND(current, other)    (((current) < (other)) ? ((current) = (other)) : (current))
# define UPPER_BOUND(current, other)    (((current) > (other)) ? ((current) = (other)) : (current))
# define BOUND(val, min, max)           (((val) < (min)) ? ((val) = (min)) : (((val) > (max)) ? ((val) = (max)) : (val)))
#endif
/** @def AT_LEAST(current, other) Alias for LOWER_BOUND().  This macro is an alias for LOWER_BOUND(). */
#define AT_LEAST(current, other)        LOWER_BOUND(current, other)
/** @def MAX_IT(current, other) Alias for LOWER_BOUND().  This macro is an alias for LOWER_BOUND(). */
#define MAX_IT(current, other)          LOWER_BOUND(current, other)
/** @def AT_MOST(current, other) Alias for UPPER_BOUND().  This macro is an alias for UPPER_BOUND(). */
#define AT_MOST(current, other)         UPPER_BOUND(current, other)
/** @def MIN_IT(current, other) Alias for UPPER_BOUND().  This macro is an alias for UPPER_BOUND(). */
#define MIN_IT(current, other)          UPPER_BOUND(current, other)
/** @def CONTAIN(val, min, max) Alias for BOUND().  This macro is an alias for BOUND(). */
#define CONTAIN(val, min, max)          BOUND(val, min, max)
/**
 * Swaps two values.
 *
 * This macro swaps the values of its first two parameters using the
 * third as temporary storage.
 *
 * @param one The first variable.
 * @param two The second variable.
 * @param tmp A temporary holding spot used during swapping.
 */
#define SWAP_IT(one, two, tmp)          do {(tmp) = (one); (one) = (two); (two) = (tmp);} while (0)

/**
 * @def SWAP(a, b)
 * Swaps two values.
 *
 * This macro performs the same task as the SWAP_IT() macro, except
 * that no temporary variable is required.  Instead, a temporary
 * variable is created by the macro itself.  Under gcc, the
 * __typeof__() extension is used to create a temporary variable of
 * the same type as @a a.  Under other compilers, a void pointer is
 * used.
 *
 * @param a The first variable.
 * @param b The second variable.
 */
#ifdef __GNUC__
# define SWAP(a, b)  (void) __extension__ ({__typeof__(a) __tmp = (a); (a) = (b); (b) = __tmp;})
#else
# define SWAP(a, b)  do {void *__tmp = ((void *)(a)); (a) = (b); (b) = __tmp;} while (0)
#endif
/**
 * Swaps two values.
 *
 * This macro swaps the values of @a a and @a b using the now-infamous
 * chained XOR trick.
 *
 * @attention ONLY use this with like variables, and only those which
 * can safely be cast to and from a long.  If you're unsure of whether
 * or not it would be safe, use SWAP() or SWAP_IT() instead!
 *
 * @param a The first variable.
 * @param b The second variable.
 */
#if STRICT_ISO_C99
#  define BINSWAP(a, b)  SWAP(a, b)
#else
#  define BINSWAP(a, b)  (((long) (a)) ^= ((long) (b)) ^= ((long) (a)) ^= ((long) (b)))
#endif

/**
 * Make sure a char pointer is non-NULL before printing it.
 *
 * This is a convenience macro primarily targetted at systems like
 * Solaris where doing a printf() on a NULL char pointer using %s
 * results in a segmentation fault rather than helpful message.  This
 * macro should be used in any place where a string is printed which
 * could potentially be NULL.
 *
 * @param x A string (char *).
 * @return @a x, or if @a x is NULL, the string "<@a x null>"
 */
#define NONULL(x) (((char *) (x)) ? ((char *) (x)) : ((char *) ("<" #x " null>")))

/**
 * Not-A-Number
 *
 * This makes sure NAN is defined.
 */
#ifndef NAN
#  ifdef MAX_FLOAT
#    define NAN MAX_FLOAT
#  elsif defined(MAXFLOAT)
#    define NAN MAX_FLOAT
#  elsif defined(HUGE)
#    define NAN HUGE
#  else
/* FIXME:  This could be dangerous...anyone have a better idea? */
#    define NAN 3.40282347e+38F
#  endif
#endif

/****************************** DEBUGGING GOOP ********************************/
#ifndef LIBAST_DEBUG_FD
/**
 * Where to send debugging output.
 *
 * This defines where debugging output should be sent.  Should be
 * either stdout or stderr.
 *
 * @ingroup DOXGRP_DEBUG
 */
# define LIBAST_DEBUG_FD  (stderr)
#endif
#ifndef DEBUG
/**
 * Maximum compile-time debugging level.
 *
 * LibAST supports debugging levels, allowing for progressively more
 * verbosity of debugging output as the level gets higher.  This
 * defines the compile-time maximum; support for higher debugging
 * levels than this will not even be compiled in, so use care when
 * setting this.
 *
 * @ingroup DOXGRP_DEBUG
 */
# define DEBUG 0
#endif

/** UNDOCUMENTED. */
#define DEBUG_LEVEL       (libast_debug_level)
/** UNDOCUMENTED. */
#define DEBUG_FLAGS       (libast_debug_flags)

/** Does nothing.  This macro is a nop (no operation).  It does nothing. */
#define NOP ((void)0)

/**
 * A fix-me NOP.
 *
 * This is the same as NOP(), but is used to mark something needing to
 * be fixed.
 */
#define FIXME_NOP(x)
/**
 * Mark a block of code needing fixing.
 *
 * This marks a block of code needing fixing and removes it.
 */
#define FIXME_BLOCK 0

/**
 * Mark unused blocks of code.
 *
 * This marks a block of code as unused and removes it.
 */
#define UNUSED_BLOCK 0

/**
 * @def __DEBUG()
 * Format and print debugging output.
 *
 * This macro formats and prints debugging output by prepending a
 * timestamp, the filename, the line number, and (if available) the
 * function name.
 *
 * This is an internal macro and should not be used directly.
 * @ingroup DOXGRP_DEBUG
 */
#if defined(__FILE__) && defined(__LINE__)
# ifdef __GNUC__
#  define __DEBUG()  fprintf(LIBAST_DEBUG_FD, "[%lu] %12s | %4d: %s(): ", (unsigned long) time(NULL), __FILE__, __LINE__, __FUNCTION__)
# else
#  define __DEBUG()  fprintf(LIBAST_DEBUG_FD, "[%lu] %12s | %4d: ", (unsigned long) time(NULL), __FILE__, __LINE__)
# endif
#else
# define __DEBUG()   NOP
#endif

/**
 * Assert reaching a line of code.
 *
 * This macro is simply a quick-and-dirty way of printing out a unique
 * message which proves that a particular portion of code was reached
 * and executed properly.
 *
 * @ingroup DOXGRP_DEBUG
 */
#define MOO()  do {__DEBUG(); libast_dprintf("Moo.\n");} while (0)

/**
 * @def ASSERT(x)
 * Asserts that a condition is true.
 *
 * This macro evaluates an expression, @a x, and takes action if the
 * expression evaluates to false (0).  It works similarly to the libc
 * function assert(), with the exception that it will not call abort()
 * if the assertion fails.  Instead, it will either issue a fatal
 * error (generally resulting in a backtrace) if debugging is active,
 * or print a warning if it is not.  In either event, the warning/error
 * message will contain the filename, line number, and (if available)
 * function name where the error occured.
 *
 * If only a warning is generated, the function will return
 * immediately.
 *
 * @param x Any valid boolean expression.
 * @ingroup DOXGRP_DEBUG
 */
/**
 * @def ASSERT_RVAL(x, val)
 * Asserts that a condition is true, and provides a return value in
 * case it isn't.
 *
 * This macro is identical to ASSERT(), except that it returns a
 * value, @a val, instead of returning void.
 *
 * @param x   Any valid boolean expression.
 * @param val The return value to use if @a x evaluates to false.
 * @ingroup DOXGRP_DEBUG
 */
/**
 * @def ASSERT_NOTREACHED()
 * Asserts that a particular piece of code is not reached.
 *
 * This macro is used in sections of code that should never be
 * reached.  Its actions are similar to those of ASSERT(), but instead
 * of evaluating an expression, it always evaluates to false.
 *
 * @ingroup DOXGRP_DEBUG
 */
/**
 * @def ASSERT_NOTREACHED_RVAL(val)
 * Asserts that a particular piece of code is not reached, and
 * provides a return value in case it is.
 *
 * This macro is identical to ASSERT_NOTREACHED(), except that it
 * returns a value, @a val, instead of returning void.
 *
 * @ingroup DOXGRP_DEBUG
 */
/**
 * @def ABORT()
 * Throw a fatal exception.
 *
 * This macro is a replacement for the libc abort() function.  This
 * version provides file/line/function information in the fatal error
 * message.
 *
 * @ingroup DOXGRP_DEBUG
 */
/**
 * @def REQUIRE(x)
 * Return if an expression is false.
 *
 * This macro is similar to ASSERT(), except that @a x evaluating to
 * false is not necessarily an error.  Normally, this macro simply
 * causes the function to return.  However, if debugging is active, a
 * message is printed noting the expression @a x and the location of
 * the failure.  This macro is often used to test preconditions, such
 * as making sure pointers are non-NULL before using them.
 *
 * @param x Any valid boolean expression.
 * @ingroup DOXGRP_DEBUG
 */
/**
 * @def REQUIRE_RVAL(x, v)
 * Return @a v if an expression is false.
 *
 * This macro is identical to REQUIRE(), except that a return value
 * for the function is supplied.
 *
 * @param x Any valid boolean expression
 * @param v The function return value to use if @a x evaluates to
 * false.
 * @ingroup DOXGRP_DEBUG
 */
#if DEBUG >= 1
# if defined(__FILE__) && defined(__LINE__)
#  ifdef __GNUC__
#   define ASSERT(x)  do {if (!(x)) {if (DEBUG_LEVEL>=1) {libast_fatal_error("ASSERT failed in %s() at %s:%d:  %s\n", __FUNCTION__, __FILE__, __LINE__, #x);} \
                                                    else {libast_print_warning("ASSERT failed in %s() at %s:%d:  %s\n", __FUNCTION__, __FILE__, __LINE__, #x); return;}}} while (0)
#   define ASSERT_RVAL(x, val)  do {if (!(x)) {if (DEBUG_LEVEL>=1) {libast_fatal_error("ASSERT failed in %s() at %s:%d:  %s\n", __FUNCTION__, __FILE__, __LINE__, #x);} \
                                                              else {libast_print_warning("ASSERT failed in %s() at %s:%d:  %s\n", __FUNCTION__, __FILE__, __LINE__, #x);} \
                                               return (val);}} while (0)
#   define ASSERT_NOTREACHED()  do {if (DEBUG_LEVEL>=1) {libast_fatal_error("ASSERT failed in %s() at %s:%d:  This code should not be reached.\n", __FUNCTION__, __FILE__, __LINE__);} \
                                                   else {libast_print_warning("ASSERT failed in %s() at %s:%d:  This code should not be reached.\n", __FUNCTION__, __FILE__, __LINE__);} \
                                    } while (0)
#   define ASSERT_NOTREACHED_RVAL(val)  do {if (DEBUG_LEVEL>=1) {libast_fatal_error("ASSERT failed in %s() at %s:%d:  This code should not be reached.\n", __FUNCTION__, __FILE__, __LINE__);} \
                                                           else {libast_print_warning("ASSERT failed in %s() at %s:%d:  This code should not be reached.\n", __FUNCTION__, __FILE__, __LINE__);} \
                                            return (val);} while (0)
#   define ABORT() libast_fatal_error("Aborting in %s() at %s:%d.\n", __FUNCTION__, __FILE__, __LINE__)
#  else
#   define ASSERT(x)  do {if (!(x)) {if (DEBUG_LEVEL>=1) {libast_fatal_error("ASSERT failed at %s:%d:  %s\n", __FILE__, __LINE__, #x);} \
                                                    else {libast_print_warning("ASSERT failed at %s:%d:  %s\n", __FILE__, __LINE__, #x); return;}}} while (0)
#   define ASSERT_RVAL(x, val)  do {if (!(x)) {if (DEBUG_LEVEL>=1) {libast_fatal_error("ASSERT failed at %s:%d:  %s\n", __FILE__, __LINE__, #x);} \
                                                              else {libast_print_warning("ASSERT failed at %s:%d:  %s\n", __FILE__, __LINE__, #x);} \
                                               return (val);}} while (0)
#   define ASSERT_NOTREACHED()  do {if (DEBUG_LEVEL>=1) {libast_fatal_error("ASSERT failed at %s:%d:  This code should not be reached.\n", __FILE__, __LINE__);} \
                                                   else {libast_print_warning("ASSERT failed at %s:%d:  This code should not be reached.\n", __FILE__, __LINE__);} \
                                    } while (0)
#   define ASSERT_NOTREACHED_RVAL(val)  do {if (DEBUG_LEVEL>=1) {libast_fatal_error("ASSERT failed at %s:%d:  This code should not be reached.\n", __FILE__, __LINE__);} \
                                                           else {libast_print_warning("ASSERT failed at %s:%d:  This code should not be reached.\n", __FILE__, __LINE__);} \
                                            return (val);} while (0)
#   define ABORT() libast_fatal_error("Aborting at %s:%d.\n", __FILE__, __LINE__)
#  endif
# else
#  define ASSERT(x)  do {if (!(x)) {if (DEBUG_LEVEL>=1) {libast_fatal_error("ASSERT failed:  %s\n", #x);} \
                                                   else {libast_print_warning("ASSERT failed:  %s\n", #x); return;}}} while (0)
#  define ASSERT_RVAL(x, val)  do {if (!(x)) {if (DEBUG_LEVEL>=1) {libast_fatal_error("ASSERT failed:  %s\n", #x);} \
                                                             else {libast_print_warning("ASSERT failed:  %s\n", #x);} return (val);}} while (0)
#  define ASSERT_NOTREACHED()           return
#  define ASSERT_NOTREACHED_RVAL(x)     return (x)
#  define ABORT()                       libast_fatal_error("Aborting.\n")
# endif
# define REQUIRE(x)                     do {if (!(x)) {if (DEBUG_LEVEL>=1) {__DEBUG(); libast_dprintf("REQUIRE failed:  %s\n", #x);} return;}} while (0)
# define REQUIRE_RVAL(x, v)             do {if (!(x)) {if (DEBUG_LEVEL>=1) {__DEBUG(); libast_dprintf("REQUIRE failed:  %s\n", #x);} return (v);}} while (0)
#else
# define ASSERT(x)                      NOP
# define ASSERT_RVAL(x, val)            NOP
# define ASSERT_NOTREACHED()            return
# define ASSERT_NOTREACHED_RVAL(val)    return (val)
# define ABORT()                        libast_fatal_error("Aborting.\n")
# define REQUIRE(x)                     do {if (!(x)) return;} while (0)
# define REQUIRE_RVAL(x, v)             do {if (!(x)) return (v);} while (0)
#endif

/**
 * @def DPRINTF(x)
 * Print debugging output.
 *
 * This macro can be used for unconditional debugging output.  If any
 * level of debugging support has been compiled in, this macro will
 * print a debugging message.
 *
 * This macro will almost never be used directly; instead, use the
 * D_*() macros.
 *
 * @attention Calls to this and other debugging output macros
 * MUST be double-parenthesized, like so:  DPRINTF((...));
 *
 * @param x A parenthesized argument list suitable for a printf-style
 *          function.
 * @see @link DOXGRP_DEBUG Debugging Subsystem @endlink
 * @ingroup DOXGRP_DEBUG
 */
/**
 * @def DPRINTF1(x)
 * Print level 1 debugging output.
 *
 * This macro is identical to DPRINTF(), except that the message will
 * only be printed if the debug level is 1 or higher.
 *
 * @param x A parenthesized argument list suitable for a printf-style
 *          function.
 * @see @link DOXGRP_DEBUG Debugging Subsystem @endlink
 * @ingroup DOXGRP_DEBUG
 */
/**
 * @def DPRINTF2(x)
 * Print level 2 debugging output.
 *
 * This macro is identical to DPRINTF(), except that the message will
 * only be printed if the debug level is 2 or higher.
 *
 * @param x A parenthesized argument list suitable for a printf-style
 *          function.
 * @see @link DOXGRP_DEBUG Debugging Subsystem @endlink
 * @ingroup DOXGRP_DEBUG
 */
/**
 * @def DPRINTF3(x)
 * Print level 3 debugging output.
 *
 * This macro is identical to DPRINTF(), except that the message will
 * only be printed if the debug level is 3 or higher.
 *
 * @param x A parenthesized argument list suitable for a printf-style
 *          function.
 * @see @link DOXGRP_DEBUG Debugging Subsystem @endlink
 * @ingroup DOXGRP_DEBUG
 */
/**
 * @def DPRINTF4(x)
 * Print level 4 debugging output.
 *
 * This macro is identical to DPRINTF(), except that the message will
 * only be printed if the debug level is 4 or higher.
 *
 * @param x A parenthesized argument list suitable for a printf-style
 *          function.
 * @see @link DOXGRP_DEBUG Debugging Subsystem @endlink
 * @ingroup DOXGRP_DEBUG
 */
/**
 * @def DPRINTF5(x)
 * Print level 5 debugging output.
 *
 * This macro is identical to DPRINTF(), except that the message will
 * only be printed if the debug level is 5 or higher.
 *
 * @param x A parenthesized argument list suitable for a printf-style
 *          function.
 * @see @link DOXGRP_DEBUG Debugging Subsystem @endlink
 * @ingroup DOXGRP_DEBUG
 */
/**
 * @def DPRINTF6(x)
 * Print level 6 debugging output.
 *
 * This macro is identical to DPRINTF(), except that the message will
 * only be printed if the debug level is 6 or higher.
 *
 * @param x A parenthesized argument list suitable for a printf-style
 *          function.
 * @see @link DOXGRP_DEBUG Debugging Subsystem @endlink
 * @ingroup DOXGRP_DEBUG
 */
/**
 * @def DPRINTF7(x)
 * Print level 7 debugging output.
 *
 * This macro is identical to DPRINTF(), except that the message will
 * only be printed if the debug level is 7 or higher.
 *
 * @param x A parenthesized argument list suitable for a printf-style
 *          function.
 * @see @link DOXGRP_DEBUG Debugging Subsystem @endlink
 * @ingroup DOXGRP_DEBUG
 */
/**
 * @def DPRINTF8(x)
 * Print level 8 debugging output.
 *
 * This macro is identical to DPRINTF(), except that the message will
 * only be printed if the debug level is 8 or higher.
 *
 * @param x A parenthesized argument list suitable for a printf-style
 *          function.
 * @see @link DOXGRP_DEBUG Debugging Subsystem @endlink
 * @ingroup DOXGRP_DEBUG
 */
/**
 * @def DPRINTF9(x)
 * Print level 9 debugging output.
 *
 * This macro is identical to DPRINTF(), except that the message will
 * only be printed if the debug level is 9 or higher.
 *
 * @param x A parenthesized argument list suitable for a printf-style
 *          function.
 * @see @link DOXGRP_DEBUG Debugging Subsystem @endlink
 * @ingroup DOXGRP_DEBUG
 */
#if DEBUG >= 1
# ifndef DPRINTF
#  define DPRINTF(x)           do { __DEBUG(); libast_dprintf x; } while (0)
# endif
# define DPRINTF1(x)           do { if (DEBUG_LEVEL >= 1) {__DEBUG(); libast_dprintf x;} } while (0)
# define DPRINTF2(x)           do { if (DEBUG_LEVEL >= 2) {__DEBUG(); libast_dprintf x;} } while (0)
# define DPRINTF3(x)           do { if (DEBUG_LEVEL >= 3) {__DEBUG(); libast_dprintf x;} } while (0)
# define DPRINTF4(x)           do { if (DEBUG_LEVEL >= 4) {__DEBUG(); libast_dprintf x;} } while (0)
# define DPRINTF5(x)           do { if (DEBUG_LEVEL >= 5) {__DEBUG(); libast_dprintf x;} } while (0)
# define DPRINTF6(x)           do { if (DEBUG_LEVEL >= 6) {__DEBUG(); libast_dprintf x;} } while (0)
# define DPRINTF7(x)           do { if (DEBUG_LEVEL >= 7) {__DEBUG(); libast_dprintf x;} } while (0)
# define DPRINTF8(x)           do { if (DEBUG_LEVEL >= 8) {__DEBUG(); libast_dprintf x;} } while (0)
# define DPRINTF9(x)           do { if (DEBUG_LEVEL >= 9) {__DEBUG(); libast_dprintf x;} } while (0)
#else
# ifndef DPRINTF
#  define DPRINTF(x)           NOP
# endif
# define DPRINTF1(x)           NOP
# define DPRINTF2(x)           NOP
# define DPRINTF3(x)           NOP
# define DPRINTF4(x)           NOP
# define DPRINTF5(x)           NOP
# define DPRINTF6(x)           NOP
# define DPRINTF7(x)           NOP
# define DPRINTF8(x)           NOP
# define DPRINTF9(x)           NOP
#endif

/**
 * Debugging output you (almost) never want.
 *
 * This macro is used for mapping debugging output you almost never
 * want to see.  Map D_*() macros to this for overly verbose or
 * problematic debugging information, then manually redefine this as
 * needed.
 *
 * @param x A parenthesized argument list suitable for a printf-style
 *          function.
 * @see @link DOXGRP_DEBUG Debugging Subsystem @endlink
 * @ingroup DOXGRP_DEBUG
 */
#define D_NEVER(x)             NOP

/** Set options debugging to level 1.  @see @link DOXGRP_DEBUG Debugging Subsystem @endlink */
#define DEBUG_OPTIONS          1
/**
 * Option debugging macro.
 *
 * This macro is used for debugging output related to the options
 * subsystem.  It maps to DPRINTF1() so that options-related debugging
 * output will occur at debug level 1 and higher.
 *
 * @see @link DOXGRP_DEBUG Debugging Subsystem @endlink
 * @ingroup DOXGRP_DEBUG
 */
#if DEBUG >= DEBUG_OPTIONS
#  define D_OPTIONS_IF           if (DEBUG_LEVEL >= DEBUG_OPTIONS)
#  define D_OPTIONS(x)           do { D_OPTIONS_IF {DPRINTF(x);} } while (0)
#else
#  define D_OPTIONS_IF           if (0)
#  define D_OPTIONS(x)           NOP
#endif

/** Set object system debugging to level 2.  @see @link DOXGRP_DEBUG Debugging Subsystem @endlink */
#define DEBUG_OBJ                2
/**
 * Object debugging macro.
 *
 * This macro is used for debugging output related to the object
 * subsystem.  It maps to DPRINTF2() so that object-related debugging
 * output will occur at debug level 2 and higher.
 *
 * @see @link DOXGRP_DEBUG Debugging Subsystem @endlink
 * @ingroup DOXGRP_DEBUG
 */
#if DEBUG >= DEBUG_OBJ
#  define D_OBJ_IF               if (DEBUG_LEVEL >= DEBUG_OBJ)
#  define D_OBJ(x)               do { D_OBJ_IF {DPRINTF(x);} } while (0)
#else
#  define D_OBJ_IF               if (0)
#  define D_OBJ(x)               NOP
#endif

/** Set config file parser debugging to level 3.  @see @link DOXGRP_DEBUG Debugging Subsystem @endlink */
#define DEBUG_CONF               3
/**
 * Config file parser debugging macro.
 *
 * This macro is used for debugging output related to the config file
 * parser.  It maps to DPRINTF3() so that config-related debugging
 * output will occur at debug level 3 and higher.
 *
 * @see @link DOXGRP_DEBUG Debugging Subsystem @endlink
 * @ingroup DOXGRP_DEBUG
 */
#if DEBUG >= DEBUG_CONF
#  define D_CONF_IF              if (DEBUG_LEVEL >= DEBUG_CONF)
#  define D_CONF(x)              do { D_CONF_IF {DPRINTF(x);} } while (0)
#else
#  define D_CONF_IF              if (0)
#  define D_CONF(x)              NOP
#endif

/** Set memory allocation debugging to level 5.  @see @link DOXGRP_DEBUG Debugging Subsystem @endlink */
#define DEBUG_MEM                5
/**
 * Memory allocation debugging macro.
 *
 * This macro is used for debugging output related to the memory
 * allocation subsystem.  It maps to DPRINTF1() so that mem-related
 * debugging output will occur at debug level 5 and higher.
 *
 * @see @link DOXGRP_DEBUG Debugging Subsystem @endlink
 * @ingroup DOXGRP_DEBUG
 */
#if DEBUG >= DEBUG_MEM
#  define D_MEM_IF               if (DEBUG_LEVEL >= DEBUG_MEM)
#  define D_MEM(x)               do { D_MEM_IF {DPRINTF(x);} } while (0)
#else
#  define D_MEM_IF               if (0)
#  define D_MEM(x)               NOP
#endif

/** Set strings module debugging to level 9999.  @see @link DOXGRP_DEBUG Debugging Subsystem @endlink */
#define DEBUG_STRINGS            9999
/**
 * String routine debugging macro.
 *
 * This macro is used for debugging output related to the string
 * manipulation subsystem.  It maps to D_NEVER() so that
 * string-related debugging output can only be activated manually.
 *
 * @see @link DOXGRP_DEBUG Debugging Subsystem @endlink
 * @ingroup DOXGRP_DEBUG
 */
#if DEBUG >= DEBUG_STRINGS
#  define D_STRINGS_IF           if (DEBUG_LEVEL >= DEBUG_STRINGS)
#  define D_STRINGS(x)           do { D_STRINGS_IF {DPRINTF(x);} } while (0)
#else
#  define D_STRINGS_IF           if (0)
#  define D_STRINGS(x)           NOP
#endif
/** Set lexer/parser debugging to level 9999.  @see @link DOXGRP_DEBUG Debugging Subsystem @endlink */
#define DEBUG_PARSE              9999
/**
 * Lexer/parser debugging macro.
 *
 * This macro is used for debugging output related to the lexer/parser
 * portion of the config parser.  It maps to D_NEVER() so that
 * parser-related debugging output can only be activated manually.
 *
 * @see @link DOXGRP_DEBUG Debugging Subsystem @endlink
 * @ingroup DOXGRP_DEBUG
 */
#if DEBUG >= DEBUG_PARSE
#  define D_PARSE_IF             if (DEBUG_LEVEL >= DEBUG_PARSE)
#  define D_PARSE(x)             do { D_PARSE_IF {DPRINTF(x);} } while (0)
#else
#  define D_PARSE_IF             if (0)
#  define D_PARSE(x)             NOP
#endif



/********************************* MEM GOOP ***********************************/
/**
 * @def MALLOC(sz)
 * Allocate @a sz bytes of memory.
 * 
 * This macro is a replacement for the libc function malloc().  It
 * allocates the specified number of bytes of memory on the heap and
 * returns a pointer to that memory location.  This macro calls libc's
 * malloc() if memory debugging is off, and spifmem_malloc() if it's
 * on.
 *
 * @param sz The size in bytes of the block of memory to allocate.
 * @return A pointer to the allocated memory.
 * @see @link DOXGRP_MEM Memory Management Subsystem @endlink
 * @ingroup DOXGRP_MEM
 */
/**
 * @def CALLOC(type, n)
 * Allocate enough memory for @a n objects of type @a type.
 * 
 * This macro is a replacement for the libc function calloc().  It
 * allocates a block of memory on the heap large enough to hold @a n
 * objects of type @a type (e.g., a @a type array of size @a n).  The
 * memory area is zeroed out prior to the pointer to it being
 * returned.  This macro calls libc's calloc() if memory debugging is
 * off and spifmem_calloc() if it's on. 
 *
 * @param type The type of object to be allocated (e.g., int).
 * @param n    The number of objects to be allocated.
 * @return A pointer to the allocated memory.
 * @see @link DOXGRP_MEM Memory Management Subsystem @endlink
 * @ingroup DOXGRP_MEM
 */
/**
 * @def REALLOC(mem, sz)
 * Resize the memory block pointed to by @a mem to @a sz bytes.
 *
 * This macro is a replacement for the libc function realloc().  It
 * changes the size of a chunk of memory previously allocated by
 * malloc() or calloc() (or, by extension, the MALLOC()/CALLOC()
 * macros) and returns a pointer to the (possibly moved) memory area.
 * This macro calls libc's realloc() if memory debugging is off and
 * spifmem_realloc() if it's on.
 *
 * @param mem The old pointer whose size will be changed.
 * @param sz  The new size, in bytes, to be allocated.
 * @return The new pointer value, which may or may not differ from the
 *         old value.
 * @see @link DOXGRP_MEM Memory Management Subsystem @endlink
 * @ingroup DOXGRP_MEM
 */
/**
 * @def FREE(ptr)
 * Free a previously-allocated memory block.
 *
 * This macro is a replacement for the libc function free().  It
 * returns the previously-allocated memory block pointed to by @a ptr
 * to the heap.  This macro calls libc's free() if memory debugging is
 * off and spifmem_free() if it's on.  The @a ptr parameter is assigned
 * the value of NULL after it has been freed.
 *
 * @param ptr The pointer to be freed.
 * @see @link DOXGRP_MEM Memory Management Subsystem @endlink
 * @ingroup DOXGRP_MEM
 */
/**
 * @def STRDUP(s)
 * Duplicate a string pointer and return a pointer to the new copy.
 *
 * This macro is a replacement for the libc function strdup().  It
 * allocates a section of memory large enough to hold the string @a s
 * (including the trailing NUL character), copies the contents of @a s
 * into the new buffer, and returns a pointer to the new copy.  This
 * macro calls libc's strdup() of memory debugging is off and
 * spifmem_strdup() if it's on.
 *
 * @param s The string to duplicate.
 * @return A pointer to the newly-created copy.
 * @see @link DOXGRP_MEM Memory Management Subsystem @endlink
 * @ingroup DOXGRP_MEM
 */
/**
 * @def MALLOC_DUMP()
 * Dumps a listing of all allocated pointers along with their sizes
 * and contents in both hex and ASCII.
 *
 * This macro is used to view the status of memory allocated via the
 * LibAST memory management system.  First the pointers used to track
 * allocated memory are dumped (that's what pointer #0 is); then, each
 * allocated pointer is dumped along with its size and contents, the
 * latter being displayed both in hexadecimal form and ASCII form.
 * Non-printable characters are replaced by dots ('.').  You can see
 * a sample of the output in the
 * @link mem_example.c memory management system example @endlink.
 *
 * @see @link DOXGRP_MEM Memory Management Subsystem @endlink
 * @ingroup DOXGRP_MEM
 */
/**
 * @def X_CREATE_PIXMAP(d, win, w, h, depth)
 * Create an X pixmap.
 *
 * This macro is a replacement for the Xlib function XCreatePixmap().
 * It creates a pixmap of the specified size and returns an X resource
 * ID for it.  This macro calls Xlib's XCreatePixmap() if memory
 * debugging is off and spifmem_x_create_pixmap() if it's on.
 *
 * @param d     The X display connection.
 * @param win   The X drawable on whose display the pixmap will be
 *              created.
 * @param w     The width in pixels of the pixmap.
 * @param h     The height in pixels of the pixmap.
 * @param depth The color depth for the pixmap.
 * @return The Pixmap ID for the new pixmap.
 * @see @link DOXGRP_MEM Memory Management Subsystem @endlink
 * @ingroup DOXGRP_MEM
 */
/**
 * @def X_FREE_PIXMAP(d, p)
 * Free the specified X pixmap.
 *
 * This macro is a replacement for the Xlib function XFreePixmap().
 * It frees the specified pixmap.  This macro calls Xlib's
 * XFreePixmap() if memory debugging is off and spifmem_x_free_pixmap()
 * if it's on.
 *
 * @param d The X display connection.
 * @param p The Pixmap to be freed.
 * @see @link DOXGRP_MEM Memory Management Subsystem @endlink
 * @ingroup DOXGRP_MEM
 */
/**
 * @def IMLIB_REGISTER_PIXMAP(p)
 * Register a pixmap generated by Imlib2 so LibAST can track it.
 *
 * Unfortunately, there is no easy way to wrap all the different ways
 * Imlib2 could conceivably create an image.  So instead, simply use
 * this macro to register the pixmaps Imlib2 creates.  Then LibAST
 * will be able to track them.  This macro calls
 * spifmem_imlib_register_pixmap() if memory debugging is on and if
 * Imlib2 support has been enabled.  Otherwise, it's a NOP().
 *
 * @param p The Pixmap Imlib2 created.
 * @see @link DOXGRP_MEM Memory Management Subsystem @endlink
 * @ingroup DOXGRP_MEM
 */
/**
 * @def IMLIB_FREE_PIXMAP(p)
 * Free a pixmap (and its mask) generated by Imlib2.
 *
 * Once an Imlib2-generated pixmap has been registered, you should
 * use this macro to free it.  It calls spifmem_imlib_free_pixmap() if
 * Imlib2 support has been enabled.  Otherwise, it's a NOP().
 *
 * @param p The Imlib2-generated Pixmap to be freed.
 * @see @link DOXGRP_MEM Memory Management Subsystem @endlink
 * @ingroup DOXGRP_MEM
 */
/**
 * @def PIXMAP_DUMP()
 * Dump a listing of allocated pixmaps.
 *
 * This macro is analogous to the MALLOC_DUMP() macro; rather than
 * dumping a list of pointers, however, it dumps a list of allocated
 * pixmaps.  Like MALLOC_DUMP(), this macro is a NOP() if memory
 * debugging support has not been compiled into LibAST.
 *
 * @see @link DOXGRP_MEM Memory Management Subsystem @endlink
 * @ingroup DOXGRP_MEM
 */
/**
 * @def X_CREATE_GC(d, win, f, gcv)
 * Create an X graphics context.
 *
 * This macro is a replacement for the Xlib function XCreateGC().  It
 * creates a graphics context (GC) object and returns its X resource
 * ID.  This macro calls Xlib's XCreateGC() if memory debugging is
 * off and spifmem_x_create_gc() if it's on.
 *
 * @param d   The X display connection.
 * @param win The X drawable on whose screen the GC will be created.
 * @param f   The GC flags noting which members of @a gcv have set
 *            values.
 * @param gcv The GCValues structure defining properties of the GC.
 * @return The ID of the new GC.
 * @see @link DOXGRP_MEM Memory Management Subsystem @endlink
 * @ingroup DOXGRP_MEM
 */
/**
 * @def X_FREE_GC(d, gc)
 * Free an X graphics context.
 *
 * This macro is a replacement for the Xlib function XFreeGC().  It
 * frees a previously allocated graphics context (GC) object.  This
 * macro calls Xlib's XFreeGC() if memory debugging is off and
 * spifmem_x_free_gc() if it's on.
 *
 * @param d  The X display connection.
 * @param gc The graphics context object to free.
 * @see @link DOXGRP_MEM Memory Management Subsystem @endlink
 * @ingroup DOXGRP_MEM
 */
/**
 * @def GC_DUMP()
 * Dump a list of allocated graphics context objects.
 *
 * This macro is analogous to the MALLOC_DUMP() macro; rather than
 * dumping a list of pointers, however, it dumps a list of allocated
 * GC's.  Like MALLOC_DUMP(), this macro is a NOP() if memory
 * debugging support has not been compiled into LibAST.
 *
 * @see @link DOXGRP_MEM Memory Management Subsystem @endlink
 * @ingroup DOXGRP_MEM
 */
/**
 * @def MALLOC_MOD
 * MALLOC() call count interval.
 *
 * LibAST has the ability to count calls to MALLOC(); this defines the
 * interval for reporting the call count.  The default is 25, meaning
 * that LibAST will print the current count every 25 calls.  Note that
 * MALLOC_CALL_DEBUG must be defined when compiling LibAST, in
 * addition to memory debugging, for this feature to work.
 *
 * @see @link DOXGRP_MEM Memory Management Subsystem @endlink
 * @ingroup DOXGRP_MEM
 */
/**
 * @def REALLOC_MOD
 * REALLOC() call count interval.
 *
 * LibAST has the ability to count calls to REALLOC(); this defines
 * the interval for reporting the call count.  The default is 25,
 * meaning that LibAST will print the current count every 25 calls.
 * Note that MALLOC_CALL_DEBUG must be defined when compiling LibAST,
 * in addition to memory debugging, for this feature to work.
 *
 * @see @link DOXGRP_MEM Memory Management Subsystem @endlink
 * @ingroup DOXGRP_MEM
 */
/**
 * @def CALLOC_MOD
 * CALLOC() call count interval.
 *
 * LibAST has the ability to count calls to CALLOC(); this defines the
 * interval for reporting the call count.  The default is 25, meaning
 * that LibAST will print the current count every 25 calls.  Note that
 * MALLOC_CALL_DEBUG must be defined when compiling LibAST, in
 * addition to memory debugging, for this feature to work.
 *
 * @see @link DOXGRP_MEM Memory Management Subsystem @endlink
 * @ingroup DOXGRP_MEM
 */
/**
 * @def FREE_MOD
 * FREE() call count interval.
 *
 * LibAST has the ability to count calls to FREE(); this defines the
 * interval for reporting the call count.  The default is 25, meaning
 * that LibAST will print the current count every 25 calls.  Note that
 * MALLOC_CALL_DEBUG must be defined when compiling LibAST, in
 * addition to memory debugging, for this feature to work.
 *
 * @see @link DOXGRP_MEM Memory Management Subsystem @endlink
 * @ingroup DOXGRP_MEM
 */
#if (DEBUG >= DEBUG_MEM)
# define MALLOC(sz)                             spifmem_malloc(SPIF_CAST(charptr) __FILE__, __LINE__, (sz))
# define CALLOC(type,n)                         spifmem_calloc(SPIF_CAST(charptr) __FILE__, __LINE__, (n), (sizeof(type)))
# define REALLOC(mem,sz)                        spifmem_realloc(SPIF_CAST(charptr) #mem, SPIF_CAST(charptr) __FILE__, __LINE__, (mem), (sz))
# define FREE(ptr)                              do { spifmem_free(SPIF_CAST(charptr) #ptr, SPIF_CAST(charptr) __FILE__, __LINE__, (ptr)); (ptr) = NULL; } while (0)
# define STRDUP(s)                              spifmem_strdup(SPIF_CAST(charptr) #s, SPIF_CAST(charptr) __FILE__, __LINE__, SPIF_CAST(charptr) (s))
# define MALLOC_DUMP()                          spifmem_dump_mem_tables()
# define X_CREATE_PIXMAP(d, win, w, h, depth)   spifmem_x_create_pixmap(SPIF_CAST(charptr) __FILE__, __LINE__, (d), (win), (w), (h), (depth))
# define X_FREE_PIXMAP(d, p)                    spifmem_x_free_pixmap(SPIF_CAST(charptr) #p, SPIF_CAST(charptr) __FILE__, __LINE__, (d), (p))
# if LIBAST_IMLIB2_SUPPORT
#  define IMLIB_REGISTER_PIXMAP(p)              spifmem_imlib_register_pixmap(SPIF_CAST(charptr) #p, SPIF_CAST(charptr) __FILE__, __LINE__, (p))
#  define IMLIB_FREE_PIXMAP(p)                  spifmem_imlib_free_pixmap(SPIF_CAST(charptr) #p, SPIF_CAST(charptr) __FILE__, __LINE__, (p))
# else
#  define IMLIB_REGISTER_PIXMAP(p)              NOP
#  define IMLIB_FREE_PIXMAP(p)                  NOP
# endif
# define PIXMAP_DUMP()                          spifmem_dump_pixmap_tables()
# define X_CREATE_GC(d, win, f, gcv)            spifmem_x_create_gc(__FILE__, __LINE__, (d), (win), (f), (gcv))
# define X_FREE_GC(d, gc)                       spifmem_x_free_gc(#gc, __FILE__, __LINE__, (d), (gc))
# define GC_DUMP()                              spifmem_dump_gc_tables()
# define MALLOC_MOD 25
# define REALLOC_MOD 25
# define CALLOC_MOD 25
# define FREE_MOD 25
#else
# define MALLOC(sz)                             malloc(sz)
# define CALLOC(type,n)                         calloc((n),(sizeof(type)))
# define REALLOC(mem,sz)                        ((sz) ? ((mem) ? (realloc((mem), (sz))) : (malloc(sz))) : ((mem) ? (free(mem), NULL) : (NULL)))
# define FREE(ptr)                              do { free(ptr); (ptr) = NULL; } while (0)
# define STRDUP(s)                              strdup((char *) s)
# define MALLOC_DUMP()                          NOP
# define X_CREATE_PIXMAP(d, win, w, h, depth)   XCreatePixmap((d), (win), (w), (h), (depth))
# define X_FREE_PIXMAP(d, p)                    XFreePixmap((d), (p))
# ifdef LIBAST_IMLIB2_SUPPORT
#  define IMLIB_REGISTER_PIXMAP(p)              NOP
#  define IMLIB_FREE_PIXMAP(p)                  imlib_free_pixmap_and_mask(p)
# else
#  define IMLIB_REGISTER_PIXMAP(p)              NOP
#  define IMLIB_FREE_PIXMAP(p)                  NOP
# endif
# define PIXMAP_DUMP()                          NOP
# define X_CREATE_GC(d, win, f, gcv)            XCreateGC((d), (win), (f), (gcv))
# define X_FREE_GC(d, gc)                       XFreeGC((d), (gc))
# define GC_DUMP()                              NOP
#endif

/* Fast memset() macro contributed by vendu */
#if !defined(SIZEOF_LONG) || (SIZEOF_LONG == 8)
/** UNDOCUMENTED */
# define MEMSET_LONG() (l |= l<<32)
#else
/** UNDOCUMENTED */
# define MEMSET_LONG() NOP
#endif

/**
 * @def MEMSET(s, c, count)
 * Initialize a memory region to a particular value.
 *
 * This macro is a replacement for the libc function memset().  It
 * initializes the memory region pointed to by @a s to the value
 * specified by @a c.  The size of the memory region is specified by
 * @a count.  Note that @a c must be a byte (char) value.
 *
 * This macro has been optimized to set as many bytes simultaneously as
 * the architecture can handle, so it should offer superior
 * performance to libc's memset() function.
 *
 * @param s     A pointer to the memory region to initialize.
 * @param c     The value to which all bytes in the block will be
 *              set.
 * @param count The size, in bytes, of the memory region.
 * @see @link DOXGRP_MEM Memory Management Subsystem @endlink
 * @ingroup DOXGRP_MEM
 */
#define MEMSET(s, c, count) do { \
    char *end = (char *)(s) + (count); \
    long l; \
    long *l_dest = (long *)(s); \
    char *c_dest; \
 \
    if (!(s)) { \
        break; \
    } \
    /* areas of less than 4 * sizeof(long) are set in 1-byte chunks. */ \
    if (((unsigned long) count) >= 4 * sizeof(long)) { \
        /* fill l with c. */ \
        l = (c) | (c)<<8; \
        l |= l<<16; \
        MEMSET_LONG(); \
 \
        /* fill in 1-byte chunks until boundary of long is reached. */ \
        if ((unsigned long)l_dest & (unsigned long)(sizeof(long) -1)) { \
            c_dest = (char *)l_dest; \
            while ((unsigned long)c_dest & (unsigned long)(sizeof(long) -1)) { \
                *(c_dest++) = (c); \
            } \
            l_dest = (long *)c_dest; \
        } \
 \
        /* fill in long-size chunks as long as possible. */ \
        while (((unsigned long) (end - (char *)l_dest)) >= sizeof(long)) { \
            *(l_dest++) = l; \
        } \
    } \
 \
    /* fill the tail in 1-byte chunks. */ \
    if ((char *)l_dest < end) { \
        c_dest = (char *)l_dest; \
        *(c_dest++) = (c); \
        while (c_dest < end) { \
            *(c_dest++) = (c); \
        } \
    } \
  } while (0)



/******************************* STRINGS GOOP *********************************/
/**
 * Returns the length of a literal string.
 *
 * This macro is like libc's strlen() function, except that it
 * requires the string parameter be a literal rather than a variable.
 * This makes calculating the string length for a literal easy without
 * incurring the speed penalty of a call to strlen().
 *
 * @param x The literal string (i.e., a fixed string in quotes, like
 *          "this.").
 * @return The length of the string.
 * @see @link DOXGRP_STRINGS String Utility Routines @endlink
 * @ingroup DOXGRP_STRINGS
 */
#define CONST_STRLEN(x)            (sizeof(x) - 1)
/**
 * Compares the beginning of a string with a literal.
 *
 * This macro, like the libc str*cmp() functions, returns an integer
 * less than, equal to, or greater than zero depending on if the
 * initial part of string @a s is found to be less than, to match, or
 * to be greater than the literal string.  Generally, this is used as
 * a boolean value (as !BEG_STRCASECMP()) to determine whether or not
 * @a s starts with @a constr or not.  Note that case is ignored, as
 * the name implies.
 *
 * @param s      The string variable to compare to.
 * @param constr A literal string representing what should be the
 *               beginning of @a s.
 * @return See above.
 * @see @link DOXGRP_STRINGS String Utility Routines @endlink
 * @ingroup DOXGRP_STRINGS
 */
#define BEG_STRCASECMP(s, constr)  (strncasecmp(SPIF_CAST_C(char *) (s), constr, CONST_STRLEN(constr)))



/******************************** CONF GOOP ***********************************/
/**
 * @def PATH_MAX
 * The maximum length of a path specifier.
 *
 * LibAST requires PATH_MAX to be properly defined.  Unfortunately,
 * some UNIX versions (namely HP-UX) define it incorrectly.  Most UNIX
 * versions support a PATH_MAX of 1024, but all must support at least
 * 255.  So if PATH_MAX is defined to be less than 255 (like HP-UX and
 * its absolutely ludicrous value of 14), LibAST forceably redefines
 * it to be 255.
 * @see @link DOXGRP_CONF Configuration File Parser @endlink
 * @ingroup DOXGRP_CONF
 */
#if defined(PATH_MAX) && (PATH_MAX < 255)
#  undef PATH_MAX
#endif
#ifndef PATH_MAX
#  define PATH_MAX 255
#endif

/**
 * Maximum length of a line in a config file.
 *
 * At no time during parsing can any line in a config file exceed this
 * length (20 kB by default).
 * @see @link DOXGRP_CONF Configuration File Parser @endlink
 * @ingroup DOXGRP_CONF
 */
#define CONFIG_BUFF                     20480

/**
 * Special flag character.
 *
 * This is the special character value passed to a config context
 * parser when the @c begin statement for that context is
 * encountered.
 *
 * @see @link DOXGRP_CONF Configuration File Parser @endlink
 * @ingroup DOXGRP_CONF
 */
#define SPIFCONF_BEGIN_CHAR                 '\001'
/**
 * Special flag character string.
 *
 * This is the string representation of SPIFCONF_BEGIN_CHAR.
 *
 * @see @link DOXGRP_CONF Configuration File Parser @endlink
 * @ingroup DOXGRP_CONF
 */
#define SPIFCONF_BEGIN_STRING               "\001"
/**
 * Special flag character.
 *
 * This is the special character value passed to a config context
 * parser when the @c end statement for that context is encountered.
 *
 * @see @link DOXGRP_CONF Configuration File Parser @endlink
 * @ingroup DOXGRP_CONF
 */
#define SPIFCONF_END_CHAR                   '\002'
/**
 * Special flag character string.
 *
 * This is the string representation of SPIFCONF_END_CHAR.
 *
 * @see @link DOXGRP_CONF Configuration File Parser @endlink
 * @ingroup DOXGRP_CONF
 */
#define SPIFCONF_END_STRING                 "\002"

/**
 * Compares boolean option value to allowed true values.
 *
 * This macro compares the value of a boolean option against the
 * acceptable boolean "true" values ("1", "on", "yes", and "true").
 *
 * @param s String value of a boolean option.
 * @return Non-zero if a match is found, zero if not.
 * @see @link DOXGRP_CONF Configuration File Parser @endlink
 * @ingroup DOXGRP_CONF
 */
#define BOOL_OPT_ISTRUE(s)  (!strcasecmp(SPIF_CHARPTR_C(s), true_vals[0]) \
                             || !strcasecmp(SPIF_CHARPTR_C(s), true_vals[1]) \
                             || !strcasecmp(SPIF_CHARPTR_C(s), true_vals[2]) \
                             || !strcasecmp(SPIF_CHARPTR_C(s), true_vals[3]))
/**
 * Compares boolean option value to allowed false values.
 *
 * This macro compares the value of a boolean option against the
 * acceptable boolean "false" values ("0", "off", "no", and "false").
 *
 * @param s String value of a boolean option.
 * @return Non-zero if a match is found, zero if not.
 * @see @link DOXGRP_CONF Configuration File Parser @endlink
 * @ingroup DOXGRP_CONF
 */
#define BOOL_OPT_ISFALSE(s) (!strcasecmp(SPIF_CHARPTR_C(s), false_vals[0]) \
                             || !strcasecmp(SPIF_CHARPTR_C(s), false_vals[1]) \
                             || !strcasecmp(SPIF_CHARPTR_C(s), false_vals[2]) \
                             || !strcasecmp(SPIF_CHARPTR_C(s), false_vals[3]))

/**
 * Skip-to-end flag.
 *
 * This symbol represents the bit in the FSS flags which specifies
 * that the parser should skip the rest of the file.
 *
 * @see @link DOXGRP_CONF_FSS File State Stack @endlink
 * @ingroup DOXGRP_CONF_FSS
 */
#define FILE_SKIP_TO_END           (0x01)
/**
 * Preprocessing flag.
 *
 * This symbol represents the bit in the FSS flags which specifies
 * that this file should be preprocessed.
 *
 * @see @link DOXGRP_CONF_FSS File State Stack @endlink
 * @ingroup DOXGRP_CONF_FSS
 */
#define FILE_PREPROC               (0x02)
/**
 * Push info for a new file onto the state stack.
 *
 * This macro adds a new file state structure to the top of the stack
 * and populates it with the information contained in the macro
 * parameters.  When a new file is opened for parsing, a call is made
 * to this macro to "push" the new file onto the top of the stack.
 *
 * @param f  The file pointer (FILE *) representing the newly-opened
 *           file.
 * @param p  The path to the newly-opened file.
 * @param o  The output file name (for preprocessing).
 * @param l  The current line number for the file.
 * @param fl The flag set for the file.
 * @see @link DOXGRP_CONF_FSS File State Stack @endlink
 * @ingroup DOXGRP_CONF_FSS
 */
#define file_push(f, p, o, l, fl)  spifconf_register_fstate(f, p, o, l, fl)
/**
 * Pop a state structure off the stack.
 *
 * This macro pops a file state structure off the top of the stack.  A
 * call to this macro occurs once the parsing of the current file is
 * completed.
 *
 * @see @link DOXGRP_CONF_FSS File State Stack @endlink
 * @ingroup DOXGRP_CONF_FSS
 */
#define file_pop()                 (fstate_idx--)
/**
 * Return the top file state structure on the stack.
 *
 * This macro is used to access the file state structure currently on
 * top of the stack.
 *
 * @return The file state structure atop the stack.
 * @see @link DOXGRP_CONF_FSS File State Stack @endlink
 * @ingroup DOXGRP_CONF_FSS
 */
#define file_peek()                (fstate[fstate_idx])
/**
 * Examine the file pointer on top of the stack.
 *
 * This macro returns the file pointer (FILE *) corresponding to the
 * file currently being parsed.
 *
 * @return The current file pointer.
 * @see @link DOXGRP_CONF_FSS File State Stack @endlink
 * @ingroup DOXGRP_CONF_FSS
 */
#define file_peek_fp()             (fstate[fstate_idx].fp)
/**
 * Examine the path of the current file.
 *
 * This macro returns the path for the file currently being parsed.
 *
 * @return The path of the current file.
 * @see @link DOXGRP_CONF_FSS File State Stack @endlink
 * @ingroup DOXGRP_CONF_FSS
 */
#define file_peek_path()           (fstate[fstate_idx].path)
/**
 * Examine the path of the current pre-processing output file.
 *
 * This macro returns the path for the preprocessing output file
 * currently being parsed.
 *
 * @return The path of the current preproc output file.
 * @see @link DOXGRP_CONF_FSS File State Stack @endlink
 * @ingroup DOXGRP_CONF_FSS
 */
#define file_peek_outfile()        (fstate[fstate_idx].outfile)
/**
 * Examine the line number of the current file.
 *
 * This macro returns the current line number within the current
 * config file.
 *
 * @return The line number of the current file.
 * @see @link DOXGRP_CONF_FSS File State Stack @endlink
 * @ingroup DOXGRP_CONF_FSS
 */
#define file_peek_line()           (fstate[fstate_idx].line)
/**
 * Check whether or not we're skipping to the end of the current
 * file.
 *
 * This macro returns zero if the current file is being parsed and
 * non-zero if the parser is skipping to its end.
 *
 * @return The skip-to-end flag for the current file.
 * @see @link DOXGRP_CONF_FSS File State Stack @endlink
 * @ingroup DOXGRP_CONF_FSS
 */
#define file_peek_skip()           (fstate[fstate_idx].flags & FILE_SKIP_TO_END)
/**
 * Check whether or not the current file was preprocessed.
 *
 * This macro returns zero if the current file was not preprocessed
 * and non-zero if it was.
 *
 * @return The preprocessing flag for the current file.
 * @see @link DOXGRP_CONF_FSS File State Stack @endlink
 * @ingroup DOXGRP_CONF_FSS
 */
#define file_peek_preproc()        (fstate[fstate_idx].flags & FILE_PREPROC)

/**
 * Set the file pointer for the current file.
 *
 * @internal
 * @param f The file pointer (FILE *).
 * @see @link DOXGRP_CONF_FSS File State Stack @endlink
 * @ingroup DOXGRP_CONF_FSS
 */
#define file_poke_fp(f)            ((fstate[fstate_idx].fp) = (f))
/**
 * Set the path for the current file.
 *
 * @internal
 * @param p The path.
 * @see @link DOXGRP_CONF_FSS File State Stack @endlink
 * @ingroup DOXGRP_CONF_FSS
 */
#define file_poke_path(p)          ((fstate[fstate_idx].path) = (p))
/**
 * Set the outfile for the current file.
 *
 * @internal
 * @param o The outfile.
 * @see @link DOXGRP_CONF_FSS File State Stack @endlink
 * @ingroup DOXGRP_CONF_FSS
 */
#define file_poke_outfile(o)       ((fstate[fstate_idx].outfile) = (o))
/**
 * Set the current line number for the current file.
 *
 * @internal
 * @param l The line number.
 * @see @link DOXGRP_CONF_FSS File State Stack @endlink
 * @ingroup DOXGRP_CONF_FSS
 */
#define file_poke_line(l)          ((fstate[fstate_idx].line) = (l))
/**
 * Set the skip-to-end flag for the current file.
 *
 * @internal
 * @see @link DOXGRP_CONF_FSS File State Stack @endlink
 * @ingroup DOXGRP_CONF_FSS
 */
#define file_skip_to_end()         ((fstate[fstate_idx].flags) |= (FILE_SKIP_TO_END))
/**
 * Set/clear the skip-to-end flag for the current file.
 *
 * @internal
 * @param s 0 to clear, non-zero to set.
 * @see @link DOXGRP_CONF_FSS File State Stack @endlink
 * @ingroup DOXGRP_CONF_FSS
 */
#define file_poke_skip(s)          do {if (s) {fstate[fstate_idx].flags |= FILE_SKIP_TO_END;} else {fstate[fstate_idx].flags &= ~(FILE_SKIP_TO_END);} } while (0)
/**
 * Set/clear the preprocessing flag for the current file.
 *
 * @internal
 * @param s 0 to clear, non-zero to set.
 * @see @link DOXGRP_CONF_FSS File State Stack @endlink
 * @ingroup DOXGRP_CONF_FSS
 */
#define file_poke_preproc(s)       do {if (s) {fstate[fstate_idx].flags |= FILE_PREPROC;} else {fstate[fstate_idx].flags &= ~(FILE_PREPROC);} } while (0)
/**
 * Set all state info for the current file.
 *
 * @internal
 * @param f  The file pointer (FILE *).
 * @param p  The file path.
 * @param o  The outfile.
 * @param l  The line number.
 * @param fl The flags.
 * @see @link DOXGRP_CONF_FSS File State Stack @endlink
 * @ingroup DOXGRP_CONF_FSS
 */
#define file_poke(f, p, o, l, fl)  do {file_poke_fp(f); file_poke_path(p); file_poke_outfile(o); file_poke_line(l); fstate[fstate_idx].flags = (fl);} while (0)

/**
 * Increment the line number for the current file.
 *
 * @internal
 * @see @link DOXGRP_CONF_FSS File State Stack @endlink
 * @ingroup DOXGRP_CONF_FSS
 */
#define file_inc_line()            (fstate[fstate_idx].line++)

/**
 * File state stack structure.
 *
 * This structure comprises the individual stack elements on the file
 * state stack.  One of these structures is present on the stack for
 * each file being parsed.
 *
 * @see @link DOXGRP_CONF_FSS File State Stack @endlink
 * @ingroup DOXGRP_CONF_FSS
 */
typedef struct file_state_struct {
    /**
     * File pointer.
     *
     * Contains an open file pointer used to read data from the
     * file.
     */
    FILE *fp;
    /**
     * File path.
     *
     * Contains the path to the file.
     */
    spif_charptr_t path;
    /**
     * Preprocessing output file.
     *
     * Contains the path to the file used for preprocessing
     * output.
     */
    spif_charptr_t outfile;
    /**
     * Line number.
     *
     * Contains the current line number for the file.
     */
    spif_uint32_t line;
    /**
     * File state flags.
     *
     * Contains the skip-to-end (FILE_SKIP_TO_END) and preprocessing
     * (FILE_PREPROC) flags for the file.
     */
    spif_uint8_t flags;
} fstate_t;

/**
 * Typedef for pointers to context handler functions.
 *
 * This function pointer type is used for variables, typecasts,
 * etc. involving context handler functions.  Context handlers must
 * accept two parameters, a char * containing either the config file
 * line or a begin/end magic string, and a void * containing state
 * information; they must return a void * which will be passed to the
 * next invocation of the handler as the aforementioned state
 * information parameter.
 *
 * @see @link DOXGRP_CONF_CTX Context Handling @endlink
 * @ingroup DOXGRP_CONF_CTX
 */
typedef spif_ptr_t (*ctx_handler_t)(spif_charptr_t, spif_ptr_t);
/**
 * Typedef for pointers to built-in functions.
 *
 * This function pointer type is used for config file built-in
 * function handlers.  LibAST supplies several built-in functions
 * which can be used in config files (%get(), %appname(), etc.);
 * client programs can add their own as well.  Built-in functions take
 * a single char * parameter, the parameter list passed to the
 * built-in function in the config file.  They return a char *, the
 * result string to substitute for the function call.
 *
 * @see @link DOXGRP_CONF Configuration File Parser @endlink
 * @ingroup DOXGRP_CONF
 */
typedef spif_charptr_t (*spifconf_func_ptr_t) (spif_charptr_t);

extern fstate_t *fstate;
extern unsigned char fstate_idx;
extern const char *true_vals[], *false_vals[];


/******************************* OPTIONS GOOP **********************************/

/*@{*/
/**
 * @name Option Flags
 * Flags for individual options.
 *
 * Each option structure (spifopt_t_struct) has a 16-bit value called
 * "flags" associated with it.  The lowest 5 bits (0-4) are for option
 * types which do not require a specific value (such as boolean).  The
 * next 5 bits (5-9) are for option types which always require a
 * value.  Bit 10 is for abstract options, which could by definition
 * go either way.  And the final 5 bits (11-15) are for
 * non-type-related information, such as flagging preparsed or
 * deprecated options.
 *
 * @ingroup DOXGRP_OPT
 */

/** No flags.  No flags. */
#define SPIFOPT_FLAG_NONE                 (0)
/** Boolean option.  This flag marks a boolean option. */
#define SPIFOPT_FLAG_BOOLEAN              (1UL << 0)
/** Counter option.  This flag marks a counter option. */
#define SPIFOPT_FLAG_COUNTER              (1UL << 1)
/** No-value type mask.  This is a bitmask to select the lower 5 bits
 *  (those which represent options where no value is required).
 */
#define SPIFOPT_FLAG_TYPEMASK_NOVALUE     (0x001f)
/** Integer option.  This flag marks an integer (numeric) option. */
#define SPIFOPT_FLAG_INTEGER              (1UL << 5)
/** String option.  This flag marks a string option. */
#define SPIFOPT_FLAG_STRING               (1UL << 6)
/** Argument list option.  This flag marks an argument list option
 *  (such as -e/--exec).  There can be one of these at most.
 */
#define SPIFOPT_FLAG_ARGLIST              (1UL << 7)
/** Value type mask.  This is a bitmask to select bits 5-9
 *  (those which represent options where a value is required).
 */
#define SPIFOPT_FLAG_TYPEMASK_VALUE       (0x03e0)
/** Abstract option.  This flag marks an abstract (client-handled)
 *  option.
 */
#define SPIFOPT_FLAG_ABSTRACT             (1UL << 10)
/** Type mask.  This is a bitmask to select all type-identifying
 *  option flag bits (0-10, inclusive).
 */
#define SPIFOPT_FLAG_TYPEMASK             (0x07ff)
/**
 * Preparsed option.  This flag marks an option which is preparsed
 * (i.e., parsed only on the pre-parse pass, which is generally done
 * before config files are read).
 */
#define SPIFOPT_FLAG_PREPARSE             (1UL << 11)
/**
 * Deprecated option.  This flag marks an option which has been
 * deprecated by the author(s) of the program.  A warning message is
 * printed whenever a deprecated option is encountered.
 */
#define SPIFOPT_FLAG_DEPRECATED           (1UL << 12)
/**
 * Array option.  This flag marks an option which, rather than taking
 * a single value of its type, takes multiple values of its type.
 * LibAST will allocate and return a NULL-terminated C-style array of
 * the given type.  The absence of this flag, except for abstract
 * options, means that multiple instances of a given flag will
 * overwrite previous values, if any.  Abstract options are entirely
 * client-handled, so this flag has no meaning there.
 */
#define SPIFOPT_FLAG_ARRAY                (1UL << 13)
/** Modifier mask.  This is a bitmask to select all the non-type
 *  flags; i.e., those which modify option behavior.
 */
#define SPIFOPT_FLAG_MODMASK              (0xf800)
/*@}*/

/*@{*/
/**
 * @name Parser Settings
 * Flags which alter the behavior of the parser itself.
 *
 * The option parser settings structure (spifopt_settings_t_struct)
 * has an 8-bit flag field which contains toggles affecting the
 * parser's internal behavior.  As a general rule, these will not be
 * flags that client programs will want to manipulate.  In the event
 * that you do wish to manipulate these flags, use the
 * SPIFOPT_FLAGS_*() macros.
 *
 * @ingroup DOXGRP_OPT
 */

/** 
 * Preparse flag.  This flag denotes whether or not the next call to
 * the spifopt_parse() function will parse only those options which
 * have the SPIFOPT_FLAG_PREPARSE flag set, after which it will clear
 * this flag.  Callers wishing to have certain options pre-parsed
 * should set this flag prior to invoking spifopt_parse().  Use of
 * this flag is not required.
 */
#define SPIFOPT_SETTING_PREPARSE         (1UL << 0)
/*@}*/

/*@{*/
/**
 * @name Option Declaration Convenience Macros
 * Macros which simplify the building of the options list.
 *
 * Each client program which intends to use the LibAST option parser
 * (i.e., calls spifopt_parse() one or more times) is responsible for
 * building its own option structure (spifopt_t) array which is then
 * registered with LibAST using the SPIFOPT_OPTLIST_SET() macro.
 *
 * To simplify the declaration of this structure, a set of convenience
 * macros is provided that will make the structure more
 * human-readable.  Although their use is not required, it is
 * recommended in order to enhance readability and reduce the
 * probability of human error.
 *
 * @ingroup DOXGRP_OPT
 */

/**
 * Primary option declaration macro.
 *
 * This is the primary macro used for declaring an option.  All other
 * option declaration convenience macros are merely simpler forms of
 * this macro.  If you prefer, you can use this macro for all option
 * declarations; just be sure to get the parameters right.
 *
 * @param s The short form as a char, or 0 for none.
 * @param l The long form as a char *. (required)
 * @param d The description as a char *. (required)
 * @param f Bitwise OR of zero or more flags.
 * @param p The pointer to where the data should be stored.  Except in
 *          the case of function pointers (for abstract options), this
 *          parameter will need the & operator.  See the other
 *          convenience macros for type requirements.
 * @param m The bitmask for a boolean option, or 0 for other types.
 */
#define SPIFOPT_OPTION(s, l, d, f, p, m)  { s, SPIF_CHARPTR(l), SPIF_CHARPTR(d), f, p, m }
/**
 * Declare a boolean option.
 *
 * This macro is used to declare a simple boolean option with both a
 * short and a long form.
 *
 * @param s The short form as a char, or 0 for none.
 * @param l The long form as a char *. (required)
 * @param d The description as a char *. (required)
 * @param v A variable of type "unsigned long" (or one that can be
 *          safely typecast to/from it) to be used as a bitfield.
 * @param m The bitmask to be set/unset within the bitfield.
 * @see SPIFOPT_OPTION()
 */
#define SPIFOPT_BOOL(s, l, d, v, m) \
    SPIFOPT_OPTION(s, SPIF_CHARPTR(l), SPIF_CHARPTR(d), (SPIFOPT_FLAG_BOOLEAN), &(v), m)
/**
 * Declare a pre-parsed boolean option.
 *
 * This macro is used to declare a boolean option with both a short
 * and a long form which will be pre-parsed.
 *
 * @param s The short form as a char, or 0 for none.
 * @param l The long form as a char *. (required)
 * @param d The description as a char *. (required)
 * @param v A variable of type "unsigned long" (or one that can be
 *          safely typecast to/from it) to be used as a bitfield.
 * @param m The bitmask to be set/unset within the bitfield.
 * @see SPIFOPT_OPTION()
 */
#define SPIFOPT_BOOL_PP(s, l, d, v, m) \
    SPIFOPT_OPTION(s, SPIF_CHARPTR(l), SPIF_CHARPTR(d), (SPIFOPT_FLAG_BOOLEAN | SPIFOPT_FLAG_PREPARSE), &(v), m)
/**
 * Declare a long-only boolean option.
 *
 * This macro is used to declare a boolean option with only a long
 * form.
 *
 * @param l The long form as a char *. (required)
 * @param d The description as a char *. (required)
 * @param v A variable of type "unsigned long" (or one that can be
 *          safely typecast to/from it) to be used as a bitfield.
 * @param m The bitmask to be set/unset within the bitfield.
 * @see SPIFOPT_OPTION()
 */
#define SPIFOPT_BOOL_LONG(l, d, v, m) \
    SPIFOPT_OPTION(0, SPIF_CHARPTR(l), SPIF_CHARPTR(d), (SPIFOPT_FLAG_BOOLEAN), &(v), m)
/**
 * Declare a long-only, pre-parsed boolean option.
 *
 * This macro is used to declare a boolean option with only a long
 * form which will be pre-parsed.
 *
 * @param l The long form as a char *. (required)
 * @param d The description as a char *. (required)
 * @param v A variable of type "unsigned long" (or one that can be
 *          safely typecast to/from it) to be used as a bitfield.
 * @param m The bitmask to be set/unset within the bitfield.
 * @see SPIFOPT_OPTION()
 */
#define SPIFOPT_BOOL_LONG_PP(l, d, v, m) \
    SPIFOPT_OPTION(0, SPIF_CHARPTR(l), SPIF_CHARPTR(d), (SPIFOPT_FLAG_BOOLEAN | SPIFOPT_FLAG_PREPARSE), &(v), m)
/**
 * Declare an integer option.
 *
 * This macro is used to declare a simple integer option with both a
 * short and a long form.
 *
 * @param s The short form as a char, or 0 for none.
 * @param l The long form as a char *. (required)
 * @param d The description as a char *. (required)
 * @param v A variable of type "int" (or one that can be
 *          safely typecast to/from it) to store the option value.
 * @see SPIFOPT_OPTION()
 */
#define SPIFOPT_INT(s, l, d, v) \
    SPIFOPT_OPTION(s, SPIF_CHARPTR(l), SPIF_CHARPTR(d), (SPIFOPT_FLAG_INTEGER), &(v), 0)
/**
 * Declare a pre-parsed integer option.
 *
 * This macro is used to declare an integer option with both a short
 * and a long form which will be pre-parsed.
 *
 * @param s The short form as a char, or 0 for none.
 * @param l The long form as a char *. (required)
 * @param d The description as a char *. (required)
 * @param v A variable of type "int" (or one that can be
 *          safely typecast to/from it) to store the option value.
 * @see SPIFOPT_OPTION()
 */
#define SPIFOPT_INT_PP(s, l, d, v) \
    SPIFOPT_OPTION(s, SPIF_CHARPTR(l), SPIF_CHARPTR(d), (SPIFOPT_FLAG_INTEGER | SPIFOPT_FLAG_PREPARSE), &(v), 0)
/**
 * Declare a long-only integer option.
 *
 * This macro is used to declare an integer option with only a long
 * form.
 *
 * @param l The long form as a char *. (required)
 * @param d The description as a char *. (required)
 * @param v A variable of type "int" (or one that can be
 *          safely typecast to/from it) to store the option value.
 * @see SPIFOPT_OPTION()
 */
#define SPIFOPT_INT_LONG(l, d, v) \
    SPIFOPT_OPTION(0, SPIF_CHARPTR(l), SPIF_CHARPTR(d), (SPIFOPT_FLAG_INTEGER), &(v), 0)
/**
 * Declare a long-only, pre-parsed integer option.
 *
 * This macro is used to declare an integer option with only a long
 * form which will be pre-parsed.
 *
 * @param l The long form as a char *. (required)
 * @param d The description as a char *. (required)
 * @param v A variable of type "int" (or one that can be
 *          safely typecast to/from it) to store the option value.
 * @see SPIFOPT_OPTION()
 */
#define SPIFOPT_INT_LONG_PP(l, d, v) \
    SPIFOPT_OPTION(0, SPIF_CHARPTR(l), SPIF_CHARPTR(d), (SPIFOPT_FLAG_INTEGER | SPIFOPT_FLAG_PREPARSE), &(v), 0)
/**
 * Declare a string option.
 *
 * This macro is used to declare a simple string option with both a
 * short and a long form.
 *
 * @param s The short form as a char, or 0 for none.
 * @param l The long form as a char *. (required)
 * @param d The description as a char *. (required)
 * @param v A variable of type "const char *" (or one that can be
 *          safely typecast to/from it) to store the option value.
 * @see SPIFOPT_OPTION()
 */
#define SPIFOPT_STR(s, l, d, v) \
    SPIFOPT_OPTION(s, SPIF_CHARPTR(l), SPIF_CHARPTR(d), (SPIFOPT_FLAG_STRING), &(v), 0)
/**
 * Declare a pre-parsed string option.
 *
 * This macro is used to declare a string option with both a short and
 * a long form which will be pre-parsed.
 *
 * @param s The short form as a char, or 0 for none.
 * @param l The long form as a char *. (required)
 * @param d The description as a char *. (required)
 * @param v A variable of type "const char *" (or one that can be
 *          safely typecast to/from it) to store the option value.
 * @see SPIFOPT_OPTION()
 */
#define SPIFOPT_STR_PP(s, l, d, v) \
    SPIFOPT_OPTION(s, SPIF_CHARPTR(l), SPIF_CHARPTR(d), (SPIFOPT_FLAG_STRING | SPIFOPT_FLAG_PREPARSE), &(v), 0)
/**
 * Declare a long-only string option.
 *
 * This macro is used to declare a string option with only a long
 * form.
 *
 * @param l The long form as a char *. (required)
 * @param d The description as a char *. (required)
 * @param v A variable of type "const char *" (or one that can be
 *          safely typecast to/from it) to store the option value.
 * @see SPIFOPT_OPTION()
 */
#define SPIFOPT_STR_LONG(l, d, v) \
    SPIFOPT_OPTION(0, SPIF_CHARPTR(l), SPIF_CHARPTR(d), (SPIFOPT_FLAG_STRING), &(v), 0)
/**
 * Declare a long-only, pre-parsed string option.
 *
 * This macro is used to declare a string option with only a long form
 * which will be pre-parsed.
 *
 * @param l The long form as a char *. (required)
 * @param d The description as a char *. (required)
 * @param v A variable of type "const char *" (or one that can be
 *          safely typecast to/from it) to store the option value.
 * @see SPIFOPT_OPTION()
 */
#define SPIFOPT_STR_LONG_PP(l, d, v) \
    SPIFOPT_OPTION(0, SPIF_CHARPTR(l), SPIF_CHARPTR(d), (SPIFOPT_FLAG_STRING | SPIFOPT_FLAG_PREPARSE), &(v), 0)
/**
 * Declare an argument list option.
 *
 * This macro is used to declare a simple argument list option with
 * both a short and a long form.
 *
 * @note Due to the nature of this option type and the fact that it
 *       can consume values to the end of the command line, only one
 *       option of this type can be declared.
 *
 * @param s The short form as a char, or 0 for none.
 * @param l The long form as a char *. (required)
 * @param d The description as a char *. (required)
 * @param p A pointer of type "char **" (or one that can be
 *          safely typecast to/from it) to store the NULL-terminated
 *          argument list.
 * @see SPIFOPT_OPTION()
 */
#define SPIFOPT_ARGS(s, l, d, p) \
    SPIFOPT_OPTION(s, SPIF_CHARPTR(l), SPIF_CHARPTR(d), (SPIFOPT_FLAG_ARGLIST), &(p), 0)
/**
 * Declare a pre-parsed argument list option.
 *
 * This macro is used to declare an argument list option with both a
 * short and a long form which will be pre-parsed.
 *
 * @note Due to the nature of this option type and the fact that it
 *       can consume values to the end of the command line, only one
 *       option of this type can be declared.
 *
 * @param s The short form as a char, or 0 for none.
 * @param l The long form as a char *. (required)
 * @param d The description as a char *. (required)
 * @param p A pointer of type "char **" (or one that can be
 *          safely typecast to/from it) to store the NULL-terminated
 *          argument list.
 * @see SPIFOPT_OPTION()
 */
#define SPIFOPT_ARGS_PP(s, l, d, p) \
    SPIFOPT_OPTION(s, SPIF_CHARPTR(l), SPIF_CHARPTR(d), (SPIFOPT_FLAG_ARGLIST | SPIFOPT_FLAG_PREPARSE), &(p), 0)
/**
 * Declare a long-only argument list option.
 *
 * This macro is used to declare an argument list option with only a
 * long form.
 *
 * @note Due to the nature of this option type and the fact that it
 *       can consume values to the end of the command line, only one
 *       option of this type can be declared.
 *
 * @param l The long form as a char *. (required)
 * @param d The description as a char *. (required)
 * @param p A pointer of type "char **" (or one that can be
 *          safely typecast to/from it) to store the NULL-terminated
 *          argument list.
 * @see SPIFOPT_OPTION()
 */
#define SPIFOPT_ARGS_LONG(l, d, p) \
    SPIFOPT_OPTION(0, SPIF_CHARPTR(l), SPIF_CHARPTR(d), (SPIFOPT_FLAG_ARGLIST), &(p), 0)
/**
 * Declare a long-only, pre-parsed argument list option.
 *
 * This macro is used to declare an argument list option with only a
 * long form which will be pre-parsed.
 *
 * @note Due to the nature of this option type and the fact that it
 *       can consume values to the end of the command line, only one
 *       option of this type can be declared.
 *
 * @param l The long form as a char *. (required)
 * @param d The description as a char *. (required)
 * @param p A pointer of type "char **" (or one that can be
 *          safely typecast to/from it) to store the NULL-terminated
 *          argument list.
 * @see SPIFOPT_OPTION()
 */
#define SPIFOPT_ARGS_LONG_PP(l, d, p) \
    SPIFOPT_OPTION(0, SPIF_CHARPTR(l), SPIF_CHARPTR(d), (SPIFOPT_FLAG_ARGLIST | SPIFOPT_FLAG_PREPARSE), &(p), 0)
/**
 * Declare an abstract option.
 *
 * This macro is used to declare a simple abstract option with both a
 * short and a long form.
 *
 * @param s The short form as a char, or 0 for none.
 * @param l The long form as a char *. (required)
 * @param d The description as a char *. (required)
 * @param f A function pointer of type "spifopt_abstract_handler_t"
 *          (or one that can be safely typecast to/from it) to handle
 *          the parsing of the option.
 * @see SPIFOPT_OPTION(), spifopt_abstract_handler_t
 */
#define SPIFOPT_ABST(s, l, d, f) \
    SPIFOPT_OPTION(s, SPIF_CHARPTR(l), SPIF_CHARPTR(d), (SPIFOPT_FLAG_ABSTRACT), SPIF_CAST(ptr) f, 0)
/**
 * Declare a pre-parsed abstract option.
 *
 * This macro is used to declare an abstract option with both a short
 * and a long form which will be pre-parsed.
 *
 * @param s The short form as a char, or 0 for none.
 * @param l The long form as a char *. (required)
 * @param d The description as a char *. (required)
 * @param f A function pointer of type "spifopt_abstract_handler_t"
 *          (or one that can be safely typecast to/from it) to handle
 *          the parsing of the option.
 * @see SPIFOPT_OPTION(), spifopt_abstract_handler_t
 */
#define SPIFOPT_ABST_PP(s, l, d, f) \
    SPIFOPT_OPTION(s, SPIF_CHARPTR(l), SPIF_CHARPTR(d), (SPIFOPT_FLAG_ABSTRACT | SPIFOPT_FLAG_PREPARSE), SPIF_CAST(ptr) f, 0)
/**
 * Declare a long-only abstract option.
 *
 * This macro is used to declare an abstract option with only a long
 * form.
 *
 * @param l The long form as a char *. (required)
 * @param d The description as a char *. (required)
 * @param f A function pointer of type "spifopt_abstract_handler_t"
 *          (or one that can be safely typecast to/from it) to handle
 *          the parsing of the option.
 * @see SPIFOPT_OPTION(), spifopt_abstract_handler_t
 */
#define SPIFOPT_ABST_LONG(l, d, f) \
    SPIFOPT_OPTION(0, SPIF_CHARPTR(l), SPIF_CHARPTR(d), (SPIFOPT_FLAG_ABSTRACT), SPIF_CAST(ptr) f, 0)
/**
 * Declare a long-only, pre-parsed abstract option.
 *
 * This macro is used to declare an abstract option with only a long
 * form which will be pre-parsed.
 *
 * @param l The long form as a char *. (required)
 * @param d The description as a char *. (required)
 * @param f A function pointer of type "spifopt_abstract_handler_t"
 *          (or one that can be safely typecast to/from it) to handle
 *          the parsing of the option.
 * @see SPIFOPT_OPTION(), spifopt_abstract_handler_t
 */
#define SPIFOPT_ABST_LONG_PP(l, d, f) \
    SPIFOPT_OPTION(0, SPIF_CHARPTR(l), SPIF_CHARPTR(d), (SPIFOPT_FLAG_ABSTRACT | SPIFOPT_FLAG_PREPARSE), SPIF_CAST(ptr) f, 0)
/*@}*/

/*@{*/
/**
 * @name Option Flag Macros
 * Macros which provide access to option flag information.
 *
 * These macros provide access to option type and flag information
 * while abstracting the actual method of storage and retrieval.  Each
 * macro takes exactly one parameter, the offset within the options
 * list.
 *
 * @ingroup DOXGRP_OPT
 */

/**
 * Type macro.
 *
 * Retrieves the type flag.  Use of this macro is discouraged; use one
 * of the other macros instead.
 *
 * @param n The index of the desired option in the options list.
 * @return  The type flag (SPIFOPT_FLAG_*)
 */
#define SPIFOPT_OPT_TYPE(n)               (SPIFOPT_OPT_FLAGS(n) & SPIFOPT_FLAG_TYPEMASK)
/**
 * Boolean option test.  Tests whether or not the given option is boolean.
 *
 * @param n The index of the desired option in the options list.
 * @return  0 if option is not boolean, non-zero if it is.
 */
#define SPIFOPT_OPT_IS_BOOLEAN(n)         (SPIFOPT_OPT_FLAGS(n) & SPIFOPT_FLAG_BOOLEAN)
/**
 * Counter option test.  Tests whether or not the given option is a counter.
 *
 * @param n The index of the desired option in the options list.
 * @return  0 if option is not a counter, non-zero if it is.
 */
#define SPIFOPT_OPT_IS_COUNTER(n)         (SPIFOPT_OPT_FLAGS(n) & SPIFOPT_FLAG_COUNTER)
/**
 * Integer option test.  Tests whether or not the given option is an integer.
 *
 * @param n The index of the desired option in the options list.
 * @return  0 if option is not an integer, non-zero if it is.
 */
#define SPIFOPT_OPT_IS_INTEGER(n)         (SPIFOPT_OPT_FLAGS(n) & SPIFOPT_FLAG_INTEGER)
/**
 * String option test.  Tests whether or not the given option is a string.
 *
 * @param n The index of the desired option in the options list.
 * @return  0 if option is not a string, non-zero if it is.
 */
#define SPIFOPT_OPT_IS_STRING(n)          (SPIFOPT_OPT_FLAGS(n) & SPIFOPT_FLAG_STRING)
/**
 * Argument list option test.  Tests whether or not the given option
 * is an argument list.
 *
 * @param n The index of the desired option in the options list.
 * @return  0 if option is not an argument list, non-zero if it is.
 */
#define SPIFOPT_OPT_IS_ARGLIST(n)         (SPIFOPT_OPT_FLAGS(n) & SPIFOPT_FLAG_ARGLIST)
/**
 * Abstract option test.  Tests whether or not the given option is abstract.
 *
 * @param n The index of the desired option in the options list.
 * @return  0 if option is not abstract, non-zero if it is.
 */
#define SPIFOPT_OPT_IS_ABSTRACT(n)        (SPIFOPT_OPT_FLAGS(n) & SPIFOPT_FLAG_ABSTRACT)
/**
 * Preparse option test.  Tests whether or not the given option is to
 * be preparsed.
 *
 * @param n The index of the desired option in the options list.
 * @return  0 if option is not to be pre-parsed, non-zero if it is.
 */
#define SPIFOPT_OPT_IS_PREPARSE(n)        (SPIFOPT_OPT_FLAGS(n) & SPIFOPT_FLAG_PREPARSE)
/**
 * Deprecated option test.  Tests whether or not the given option is deprecated.
 *
 * @param n The index of the desired option in the options list.
 * @return  0 if option is not deprecated, non-zero if it is.
 */
#define SPIFOPT_OPT_IS_DEPRECATED(n)      (SPIFOPT_OPT_FLAGS(n) & SPIFOPT_FLAG_DEPRECATED)
/**
 * Array option test.  Tests whether or not the given option is an array.
 *
 * @param n The index of the desired option in the options list.
 * @return  0 if option is not an array, non-zero if it is.
 */
#define SPIFOPT_OPT_IS_ARRAY(n)           (SPIFOPT_OPT_FLAGS(n) & SPIFOPT_FLAG_ARRAY)
/**
 * Value required option test.  Tests whether or not the given option
 * requires a value.
 *
 * @param n The index of the desired option in the options list.
 * @return  0 if option does not require a value, non-zero if it does.
 */
#define SPIFOPT_OPT_NEEDS_VALUE(n)        (SPIFOPT_OPT_FLAGS(n) & SPIFOPT_FLAG_TYPEMASK_VALUE)
/*@}*/

/*@{*/
/**
 * @name Option Structure Access Macros
 * Macros which provide access to individual structure components.
 *
 * These macros provide an easy method for accessing the various
 * members of a single option structure (spifopt_t_struct) within the
 * options list without needing to know how the options list, or the
 * options within it, are stored.
 *
 * @ingroup DOXGRP_OPT
 */

/**
 * Short form.  Returns the short form of the option.
 *
 * @param n The index of the desired option in the options list.
 * @return  The short (char) form of the option.
 */
#define SPIFOPT_OPT_SHORT(n)              (SPIFOPT_OPTLIST_GET_OPT(n).short_opt)
/**
 * Long form.  Returns the long form of the option.
 *
 * @param n The index of the desired option in the options list.
 * @return  The long (const char *) form of the option.
 */
#define SPIFOPT_OPT_LONG(n)               (SPIFOPT_OPTLIST_GET_OPT(n).long_opt)
/**
 * Description.  Returns the description of the option.
 *
 * @param n The index of the desired option in the options list.
 * @return  The description (const char *) of the option.
 */
#define SPIFOPT_OPT_DESC(n)               (SPIFOPT_OPTLIST_GET_OPT(n).desc)
/**
 * Flags.  Returns the flags for the option.
 *
 * @param n The index of the desired option in the options list.
 * @return  The flags (spif_uint16_t) for the option.
 */
#define SPIFOPT_OPT_FLAGS(n)              (SPIFOPT_OPTLIST_GET_OPT(n).flags)
/**
 * Value pointer.  Returns the value pointer for the option.
 *
 * @param n The index of the desired option in the options list.
 * @return  The value pointer (void *) for the option.
 */
#define SPIFOPT_OPT_VALUE(n)              (SPIFOPT_OPTLIST_GET_OPT(n).value)
/**
 * Bitmask.  Returns the bitmask for the option.
 *
 * @param n The index of the desired option in the options list.
 * @return  The bitmask (spif_uint32_t) for the option.
 */
#define SPIFOPT_OPT_MASK(n)               (SPIFOPT_OPTLIST_GET_OPT(n).mask)
/*@}*/

/*@{*/
/**
 * @name Option Parser Settings Macros
 * Macros which provide access to the option parser settings.
 *
 * These macros provide abstracted access to various settings used by
 * the option parser.
 *
 * @ingroup DOXGRP_OPT
 */

/**
 * Retrieves a single option structure.
 *
 * This macro returns a single option structure (spifopt_t_struct)
 * from the options list.  This is mostly for internal use.
 *
 * @param n The index of the desired option in the options list.
 * @return  The option structure (spifopt_t_struct).
 */
#define SPIFOPT_OPTLIST_GET_OPT(n)        (spifopt_settings.opt_list[((n) < (spifopt_settings.num_opts) ? (n) : (0))])
/**
 * Sets the option list.
 *
 * This macro should be called by client programs to set the option
 * list.  It must be a spifopt_t * of some type (probably a
 * spifopt_t []).  Once set, it must not be manipulated directly; only
 * via the LibAST-supplied macros.
 *
 * @param l The options list variable (a spifopt_t pointer or array).
 */
#define SPIFOPT_OPTLIST_SET(l)            (spifopt_settings.opt_list = ((spifopt_t *) (l)))
/**
 * Obtains the number of options.
 *
 * This macro returns the total number of options in the options list.
 *
 * @return  The number of options in the list.
 */
#define SPIFOPT_NUMOPTS_GET()             (spifopt_settings.num_opts)
/**
 * Sets the number of options.
 *
 * This macro sets the number of options in the options list.  It
 * should be called immediately after SPIFOPT_OPTLIST_SET() and
 * @em before any functions or macros which use the options list.  The
 * most common way to handle this is to use a fixed array for the
 * options list and call
 *
 * @code
 * SPIFOPT_NUMOPTS_SET(sizeof(option_list) / sizeof(spifopt_t))
 * @endcode
 *
 * to set the option count.
 *
 * @param n The number of elements in the options list.
 */
#define SPIFOPT_NUMOPTS_SET(n)            (spifopt_settings.num_opts = (n))
/**
 * Obtains the option parser flag settings.
 *
 * This macro returns the value of the parser settings flags.  In most
 * cases, you should use SPIFOPT_FLAGS_IS_SET() instead.
 *
 * @return  The value of the parser settings flags.
 */
#define SPIFOPT_FLAGS_GET()               (spifopt_settings.flags)
/**
 * Sets an option parser settings flag.
 *
 * This macro sets one or more parser settings flags.  There are
 * currently no client-managed parser flags, so you should avoid this
 * macro.
 *
 * @param m The flag (or set of bitwise-or'd flags) to turn on.
 */
#define SPIFOPT_FLAGS_SET(m)              (spifopt_settings.flags |= (m))
/**
 * Checks whether or not an option parser settings flag is set.
 *
 * This macro tests one or more parser settings flags.  There are
 * currently no client-managed parser flags, so you should avoid this
 * macro.
 *
 * @param m The flag (or set of bitwise-or'd flags) to check.
 * @return  0 if none of the selected flags are set, non-zero if at
 *          least one is.
 */
#define SPIFOPT_FLAGS_IS_SET(m)           (spifopt_settings.flags & (m))
/**
 * Clears an option parser settings flag.
 *
 * This macro clears one or more parser settings flags.  There are
 * currently no client-managed parser flags, so you should avoid this
 * macro.
 *
 * @param m The flag (or set of bitwise-or'd flags) to turn off.
 */
#define SPIFOPT_FLAGS_CLEAR(m)            (spifopt_settings.flags &= ~(m))
/**
 * Gets the bad option count.
 *
 * This macro retrieves the count of bad options encountered during
 * option parsing.
 *
 * @return The number of bad options encountered.
 */
#define SPIFOPT_BADOPTS_GET()             (spifopt_settings.bad_opts)
/**
 * Sets the bad option count.
 *
 * This macro sets the count of bad options encountered during option
 * parsing.  This macro should not be used by client programs.
 *
 * @param n The number of bad options encountered.
 */
#define SPIFOPT_BADOPTS_SET(n)            (spifopt_settings.bad_opts = (n))
/**
 * Gets the bad option setting.
 *
 * This macro retrieves the setting for the number of bad options
 * allowed before parsing is aborted and the help screen is displayed.
 *
 * @return The number of bad options allowed.
 */
#define SPIFOPT_ALLOWBAD_GET()            (spifopt_settings.allow_bad)
/**
 * Sets the bad option setting.
 *
 * This macro sets the number of bad options allowed before parsing is
 * aborted and the help screen is displayed.  This macro should be
 * called prior to option parsing.
 *
 * @param n The number of bad options allowed.
 */
#define SPIFOPT_ALLOWBAD_SET(n)           (spifopt_settings.allow_bad = (n))
/**
 * Gets the help handler function pointer.
 *
 * This macro retrieves the pointer to the function which will be
 * called to display program help, due either to a help option or
 * an excess number of option parsing errors.  If this value is not
 * set by the client (via the SPIFOPT_HELPHANDLER_SET() macro), the
 * default handler is the built-in spifopt_usage() function.
 *
 * @return The function pointer for the help/usage screen function.
 */
#define SPIFOPT_HELPHANDLER               ((spifopt_settings.help_handler) ? (spifopt_settings.help_handler) : (spifopt_usage))
/**
 * Sets the help handler function pointer.
 *
 * This macro sets the pointer to the function which will be called to
 * display program help, due either to a help option or an excess
 * number of option parsing errors.  If the client fails to call this
 * macro prior to option parsing, the built-in spifopt_usage()
 * function is used instead.
 *
 * @param f A function pointer of type spifopt_helphandler_t.
 */
#define SPIFOPT_HELPHANDLER_SET(f)        (spifopt_settings.help_handler = (f))
/*@}*/

/**
 * @name Type Definitions
 *
 */
/*@{*/
/**
 * Typedef for help handler function.
 *
 * This type is used for declaring/typecasting function pointers which
 * will be used for help handlers.  Functions used for this should be
 * declared as returning void (and in reality does not return at all)
 * and should take either no parameters, or a single char * parameter.
 *
 * @see @link DOXGRP_OPT Command Line Option Parser @endlink, SPIFOPT_HELPHANDLER_SET()
 * @ingroup DOXGRP_OPT
 */
typedef void (*spifopt_helphandler_t)();
/**
 * Typedef for abstract option handler function.
 *
 * This type is used for declaring/typecasting function pointers which
 * will be used for abstract option handlers.  Abstract options are
 * those which require special handling; LibAST implements this by
 * allowing for an arbitrary user-specified function be invoked when
 * such an option is encountered.  Functions used for this should be
 * declared as returning void and should take a single char *
 * parameter (the value of the option, or NULL if it had no value).
 *
 * @see @link DOXGRP_OPT Command Line Option Parser @endlink
 * @ingroup DOXGRP_OPT
 */
typedef void (*spifopt_abstract_handler_t)(spif_charptr_t);

/**
 * Option structure.
 *
 * This is the structure that holds the data for each of the command
 * line options for which the parser will be looking.  Client programs
 * must create an array of these structures (a spifopt_t []) and use
 * the SPIFOPT_OPTLIST_SET() macro to tell LibAST which variable it
 * is.
 *
 * @note This structure and its members should NEVER be accessed
 * directly; they are documented solely for informational purposes.
 * The SPIFOPT_* convenience macros provide a streamlined, easy-to-use
 * abstraction layer for declaring the option list, setting option
 * parser parameters, and so forth.  Even the internal code uses these
 * macros!  Consult the macro documentation and the example code for
 * further assistance.
 *
 * @see @link DOXGRP_OPT Command Line Option Parser @endlink, @link opt_example.c example code @endlink
 * @ingroup DOXGRP_OPT
 */
typedef struct spifopt_t_struct {
    /**
     * Short form.
     *
     * The short (one char) form of the option.
     */
    spif_char_t short_opt;
    /**
     * Long form.
     *
     * The long (string) form of the option.
     */
    spif_charptr_t long_opt;
    /**
     * Description.
     *
     * The (brief) description of the option for the help screen.
     */
    spif_charptr_t desc;
    /**
     * Option type/attribute flags.
     *
     * The type and attribute flags for this option.
     */
    spif_uint16_t flags;
    /**
     * Value pointer.
     *
     * A pointer to where the value for this option should be stored.
     * Its exact type, and how it is interpreted, depends on the type
     * of option being defined.
     */
    void *value;
    /**
     * Boolean bitmask.
     *
     * For boolean options, this is the bitmask for the option.  For
     * other option types, it has no meaning.
     */
    spif_uint32_t mask;
} spifopt_t;

/**
 * Option parser settings structure.
 *
 * This is the structure that holds the settings and other internal
 * variables which control how the options parser functions.
 *
 * @note This structure and its members should NEVER be accessed
 * directly; they are documented solely for informational purposes.
 * The SPIFOPT_* convenience macros provide a streamlined, easy-to-use
 * abstraction layer for declaring the option list, setting option
 * parser parameters, and so forth.  Even the internal code uses these
 * macros!  Consult the macro documentation and the example code for
 * further assistance.
 *
 * @see @link DOXGRP_OPT Command Line Option Parser @endlink, @link opt_example.c example code @endlink
 * @ingroup DOXGRP_OPT
 */
typedef struct spifopt_settings_t_struct {
    /**
     * Options list.
     *
     * The array of option structures defining the options to look
     * for.
     */
    spifopt_t *opt_list;
    /**
     * Option count.
     *
     * The total number of options in the options list.
     */
    spif_uint16_t num_opts;
    /**
     * Parser flags.
     *
     * Flags which control the behavior of the parser.
     */
    spif_uint8_t flags;
    /**
     * Bad option count.
     *
     * Keeps track of the number of bad options (i.e., option syntax
     * errors, such as missing values or unknown options)
     * encountered.
     */
    spif_uint8_t bad_opts;
    /**
     * Bad option limit.
     *
     * The maximum number of bad options allowed before giving up and
     * displaying the help text.
     */
    spif_uint8_t allow_bad;
    spif_uint8_t indent; /**< Unused. */
    /**
     * Help handler.
     *
     * Pointer to the function which is responsible for displaying the
     * help text.  If undefined, spifopt_usage() is used.
     */
    spifopt_helphandler_t help_handler;
} spifopt_settings_t;
/*@}*/

extern spifopt_settings_t spifopt_settings;


/******************************* HASHING GOOP *********************************/

/**
 * @name Jenkins Hash
 *
 * Bob Jenkins' hash algorithm as published in December 1996.  Public
 * domain.  See http://burtleburtle.net/bob/hash/
 */
/*@{*/
/**
 * Calculate number of hash buckets needed for an n-bit hash key.
 *
 * This macro returns the number of hash buckets needed for a hash key
 * of n distinct bits.  Choose n based on your tolerable level of
 * collisions, on average, for 2^n key values.
 *
 * @param n The number of bits.
 * @return  Number of hash buckets required.
 *
 */
#define SPIFHASH_SIZE(n)       (SPIF_CAST(uint32) (1UL << (n)))

/**
 * Calculate mask to apply to hash key to get lowest n bits.
 *
 * This macro returns the bitmask needed for a hash key of n distinct
 * bits.  Choose n based on your tolerable level of collisions, on
 * average, for 2^n key values.
 *
 * @param n The number of bits.
 * @return  Bitmask to zero out all but the n lowest bits.
 *
 */
#define SPIFHASH_MASK(n)       (SPIF_CAST(uint32) (SPIFHASH_SIZE(n) - 1))

/**
 * Mix 3 32-bit integer values in a reproducible, reversible manner.
 *
 * This macro is used by the Jenkins hash algorithm to shuffle bits of
 * three integers in such a way as to make sure that changes are
 * propogated throughout the values.
 *
 * Instructions are arranged in such a way as to be parallizable;
 * i.e., excluding the first two instructions, each subsequent pair of
 * instructions may be evaluated simultaneously for pipelining
 * purposes.
 *
 * @param a A 32-bit integer.
 * @param b A 32-bit integer.
 * @param c A 32-bit integer.
 */
#define SPIFHASH_JENKINS_MIX(a,b,c) \
{ \
    a -= b; a -= c; a ^= (c>>13); \
    b -= c; b -= a; b ^= (a<<8);  \
    c -= a; c -= b; c ^= (b>>13); \
    a -= b; a -= c; a ^= (c>>12); \
    b -= c; b -= a; b ^= (a<<16); \
    c -= a; c -= b; c ^= (b>>5);  \
    a -= b; a -= c; a ^= (c>>3);  \
    b -= c; b -= a; b ^= (a<<10); \
    c -= a; c -= b; c ^= (b>>15); \
}

/**
 * A pointer to a hash function.
 *
 * This type is used to refer to any of the spifhash_* functions.  A
 * variable of this type can point to any of the built-in hash
 * functions in LibAST interchangeably.
 *
 */
typedef spif_uint32_t (*spifhash_func_t)(spif_uint8_t *, spif_uint32_t, spif_uint32_t);
/*@}*/



/******************************** PROTOTYPES **********************************/

/* msgs.c */
extern void libast_set_program_name(const char *);
extern void libast_set_program_version(const char *);
extern int libast_dprintf(const char *, ...);
extern void libast_print_error(const char *fmt, ...);
extern void libast_print_warning(const char *fmt, ...);
extern void libast_fatal_error(const char *fmt, ...);

/* debug.c */
extern unsigned int DEBUG_LEVEL;

/* mem.c */
extern void spifmem_init(void);
extern void *spifmem_malloc(const spif_charptr_t, unsigned long, size_t);
extern void *spifmem_realloc(const spif_charptr_t, const spif_charptr_t, unsigned long, void *, size_t);
extern void *spifmem_calloc(const spif_charptr_t, unsigned long, size_t, size_t);
extern void spifmem_free(const spif_charptr_t, const spif_charptr_t, unsigned long, void *);
extern spif_charptr_t spifmem_strdup(const spif_charptr_t, const spif_charptr_t,
                                     unsigned long, const spif_charptr_t);
extern void spifmem_dump_mem_tables(void);
#if LIBAST_X11_SUPPORT
extern Pixmap spifmem_x_create_pixmap(const spif_charptr_t, unsigned long, Display *,
                                      Drawable, unsigned int, unsigned int, unsigned int);
extern void spifmem_x_free_pixmap(const spif_charptr_t, const spif_charptr_t,
                                  unsigned long, Display *, Pixmap);
# if LIBAST_IMLIB2_SUPPORT
extern void spifmem_imlib_register_pixmap(const spif_charptr_t var, const spif_charptr_t filename,
                                          unsigned long line, Pixmap p);
extern void spifmem_imlib_free_pixmap(const spif_charptr_t var, const spif_charptr_t filename,
                                      unsigned long line, Pixmap p);
# endif
extern void spifmem_dump_pixmap_tables(void);
extern GC spifmem_x_create_gc(const spif_charptr_t, unsigned long, Display *, Drawable,
                              unsigned long, XGCValues *);
extern void spifmem_x_free_gc(const spif_charptr_t, const spif_charptr_t, unsigned long, Display *, GC);
extern void spifmem_dump_gc_tables(void);
#endif
extern void spiftool_free_array(void *, size_t);

/* file.c */
extern int spiftool_temp_file(spif_charptr_t, size_t);

/* strings.c */
extern spif_bool_t spiftool_safe_strncpy(spif_charptr_t dest, const spif_charptr_t src, spif_int32_t size);
extern spif_bool_t spiftool_safe_strncat(spif_charptr_t dest, const spif_charptr_t src, spif_int32_t size);
extern spif_charptr_t spiftool_substr(const spif_charptr_t, spif_int32_t, spif_int32_t);
#if LIBAST_REGEXP_SUPPORT_POSIX && HAVE_REGEX_H
extern spif_bool_t spiftool_regexp_match(const spif_charptr_t, const spif_charptr_t);
extern spif_bool_t spiftool_regexp_match_r(const spif_charptr_t str, const spif_charptr_t pattern, regex_t **rexp);
#endif
extern spif_charptr_t *spiftool_split(const spif_charptr_t, const spif_charptr_t);
extern spif_charptr_t *spiftool_split_regexp(const spif_charptr_t, const spif_charptr_t);
extern spif_charptr_t spiftool_join(spif_charptr_t, spif_charptr_t *);
extern spif_charptr_t spiftool_get_word(unsigned long, const spif_charptr_t);
extern spif_charptr_t spiftool_get_pword(unsigned long, const spif_charptr_t);
extern unsigned long spiftool_num_words(const spif_charptr_t);
extern spif_charptr_t spiftool_chomp(spif_charptr_t);
extern spif_charptr_t spiftool_downcase_str(spif_charptr_t);
extern spif_charptr_t spiftool_upcase_str(spif_charptr_t);
extern spif_charptr_t spiftool_safe_str(spif_charptr_t, unsigned short);
extern spif_charptr_t spiftool_condense_whitespace(spif_charptr_t);
extern void spiftool_hex_dump(void *, size_t);
extern spif_cmp_t spiftool_version_compare(spif_charptr_t, spif_charptr_t);
#if !(HAVE_MEMMEM)
extern void *memmem(const void *, size_t, const void *, size_t);
#endif
#if !(HAVE_STRNLEN)
extern size_t strnlen(const char *, size_t);
#endif
#if !(HAVE_USLEEP)
extern void usleep(unsigned long);
#endif
#if !(HAVE_SNPRINTF)
extern int vsnprintf(char *str, size_t count, const char *fmt, va_list args);
extern int snprintf(char *str, size_t count, const char *fmt, ...);
#endif
#if !(HAVE_STRCASESTR)
extern char *strcasestr(const char *, const char *);
#endif
#if !(HAVE_STRCASECHR)
extern char *strcasechr(const char *, const char);
#endif
#if !(HAVE_STRCASEPBRK)
extern char *strcasepbrk(const char *, const char *);
#endif
#if !(HAVE_STRREV)
extern char *strrev(char *);
#endif
#if !(HAVE_STRSEP)
extern char *strsep(char **, char *);
#endif

/* builtin_hashes.c */
extern spif_uint32_t spifhash_jenkins(register spif_uint8_t *key, register spif_uint32_t length, register spif_uint32_t seed);
extern spif_uint32_t spifhash_jenkins32(spif_uint8_t *key, register spif_uint32_t length, register spif_uint32_t seed);
#if WORDS_BIGENDIAN
#  define spifhash_jenkinsLE(k, l, s)  spifhash_jenkins((k), (l), (s))
#else
extern spif_uint32_t spifhash_jenkinsLE(register spif_uint8_t *key, register spif_uint32_t length, register spif_uint32_t seed);
#endif
extern spif_uint32_t spifhash_rotating(spif_uint8_t *key, spif_uint32_t len, spif_uint32_t seed);
extern spif_uint32_t spifhash_one_at_a_time(spif_uint8_t *key, spif_uint32_t len, spif_uint32_t seed);
extern spif_uint32_t spifhash_fnv(spif_uint8_t *key, spif_uint32_t len, spif_uint32_t seed);

/* conf.c */
extern void spifconf_init_subsystem(void);
extern unsigned char spifconf_register_context(spif_charptr_t name, ctx_handler_t handler);
extern unsigned char spifconf_register_fstate(FILE *fp, spif_charptr_t path, spif_charptr_t outfile, unsigned long line, unsigned char flags);
extern unsigned char spifconf_register_builtin(char *name, spifconf_func_ptr_t ptr);
extern unsigned char spifconf_register_context_state(unsigned char ctx_id);
extern void spifconf_free_subsystem(void);
extern spif_charptr_t spifconf_shell_expand(spif_charptr_t);
extern spif_charptr_t spifconf_find_file(const spif_charptr_t file, const spif_charptr_t dir, const spif_charptr_t pathlist);
extern FILE *spifconf_open_file(spif_charptr_t name);
extern void spifconf_parse_line(FILE *fp, spif_charptr_t buff);
extern spif_charptr_t spifconf_parse(spif_charptr_t conf_name, const spif_charptr_t dir, const spif_charptr_t path);

/* options.c */
extern void spifopt_parse(int, char **);
extern void spifopt_usage(void);


/* Do we, or do we not, pollute the namespace like we used to? */
#if LIBAST_COMPAT_05_API
/* The application must have defined this. */
/* conf.c */
# define CONF_BEGIN_CHAR                                         SPIFCONF_BEGIN_CHAR
# define CONF_END_CHAR                                           SPIFCONF_END_CHAR
# define CONF_BEGIN_STRING                                       SPIFCONF_BEGIN_STRING
# define CONF_END_STRING                                         SPIFCONF_END_STRING
typedef spifconf_func_ptr_t conf_func_ptr_t;
# define conf_init_subsystem()                                   spifconf_init_subsystem()
# define conf_register_context(a, b)                             spifconf_register_context((a), (b))
# define conf_register_fstate(a, b, c, d, e)                     spifconf_register_fstate((a), (b), (c), (d), (e))
# define conf_register_builtin(a, b)                             spifconf_register_builtin((a), (b))
# define conf_register_context_state(a)                          spifconf_register_context_state(a)
# define conf_free_subsystem()                                   spifconf_free_subsystem()
# define shell_expand(a)                                         spifconf_shell_expand(a)
# define conf_find_file(a, b, c)                                 spifconf_find_file((a), (b), (c))
# define open_config_file(a)                                     spifconf_open_file(a)
# define conf_parse_line(a, b)                                   spifconf_parse_line((a), (b))
# define conf_parse(a, b, c)                                     spifconf_parse((a), (b), (c))

/* mem.c */
# define memrec_init()                                           spifmem_init()
# define libast_malloc(a, b, c)                                  spifmem_malloc((a), (b), (c))
# define libast_realloc(a, b, c, d, e)                           spifmem_realloc((a), (b), (c), (d), (e))
# define libast_calloc(a, b, c, d)                               spifmem_calloc((a), (b), (c), (d))
# define libast_free(a, b, c, d)                                 spifmem_free((a), (b), (c), (d))
# define libast_strdup(a, b, c, d)                               spifmem_strdup((a), (b), (c), (d))
# define libast_dump_mem_tables()                                spifmem_dump_mem_tables()
# if LIBAST_X11_SUPPORT
#  define libast_x_create_pixmap(a, b, c, d, e, f, g)            spifmem_x_create_pixmap((a), (b), (c), (d), (e), (f), (g))
#  define libast_x_free_pixmap(a, b, c, d, e)                    spifmem_x_free_pixmap((a), (b), (c), (d), (e))
#  if LIBAST_IMLIB2_SUPPORT
#   define libast_imlib_register_pixmap(a, b, c, d)              spifmem_imlib_register_pixmap((a), (b), (c), (d))
#   define libast_imlib_free_pixmap(a, b, c, d)                  spifmem_imlib_free_pixmap((a), (b), (c), (d))
#  endif
#  define libast_dump_pixmap_tables()                            spifmem_dump_pixmap_tables()
#  define libast_x_create_gc(a, b, c, d, e, f)                   spifmem_x_create_gc((a), (b), (c), (d), (e), (f))
#  define libast_x_free_gc(a, b, c, d, e)                        spifmem_x_free_gc((a), (b), (c), (d), (e))
#  define libast_dump_gc_tables()                                spifmem_dump_gc_tables()
# endif
# define free_array(a, b)                                        spiftool_free_array((a), (b))

/* file.c */
# define libast_temp_file(a, b)                                  spiftool_temp_file((a), (b))

/* msgs.c */
static void (*print_error)(const char *, ...) = libast_print_error;
static void (*print_warning)(const char *, ...) = libast_print_warning;
static void (*fatal_error)(const char *, ...) = libast_fatal_error;

/* strings.c */
# define regexp_match(a, b)                                      spiftool_regexp_match((a), (b))
# define regexp_match_r(a, b, c)                                 spiftool_regexp_match_r((a), (b), (c))
# define split(a, b)                                             spiftool_split((a), (b))
# define join(a, b)                                              spiftool_join((a), (b))
# define get_word(a, b)                                          spiftool_get_word((a), (b))
# define get_pword(a, b)                                         spiftool_get_pword((a), (b))
# define num_words(a)                                            spiftool_num_words(a)
# define chomp(a)                                                spiftool_chomp(a)
# define strip_whitespace(a)                                     spiftool_strip_whitespace(a)
# define downcase_str(a)                                         spiftool_downcase_str(a)
# define upcase_str(a)                                           spiftool_upcase_str(a)
# define safe_str(a, b)                                          spiftool_safe_str((a), (b))
# define condense_whitespace(a)                                  spiftool_condense_whitespace(a)
# define hex_dump(a, b)                                          spiftool_hex_dump((a), (b))
# define version_compare(a, b)                                   spiftool_version_compare((a), (b))

#endif /* LIBAST_COMPAT_05_API */

#endif /* _LIBAST_H_ */
