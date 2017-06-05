/*
 * Copyright 2004-2007 James Bursa <bursa@users.sourceforge.net>
 * Copyright 2004 John Tytgat <joty@netsurf-browser.org>
 *
 * This file is part of NetSurf, http://www.netsurf-browser.org/
 *
 * NetSurf is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; version 2 of the License.
 *
 * NetSurf is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

/**
 * \file utils/utils.h
 * \brief Interface to a number of general purpose functionality.
 * \todo Many of these functions and macros should have their own headers.
 */

#ifndef _NETSURF_UTILS_UTILS_H_
#define _NETSURF_UTILS_UTILS_H_

#include <inttypes.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/time.h>
#include <regex.h>
#include <assert.h>
#include <stdarg.h>

#include "utils/errors.h"

/** Rectangle coordinates */
struct rect {
	int x0, y0; /**< Top left */
	int x1, y1; /**< Bottom right */
};

struct dirent;

#ifndef NOF_ELEMENTS
#define NOF_ELEMENTS(array) (sizeof(array)/sizeof(*(array)))
#endif

#ifndef ABS
#define ABS(x) (((x)>0)?(x):(-(x)))
#endif

#ifdef __MINT__ /* avoid using GCCs builtin min/max functions */
#undef min
#undef max
#endif

#ifndef min
#define min(x,y) (((x)<(y))?(x):(y))
#endif

#ifndef max
#define max(x,y) (((x)>(y))?(x):(y))
#endif

#ifndef PRIxPTR
#define PRIxPTR "x"
#endif

#ifndef PRId64
#define PRId64 "lld"
#endif

/* Windows does not have POSIX formating codes or mkdir so work around that */
#if defined(_WIN32)
#define SSIZET_FMT "Iu"
#define nsmkdir(dir, mode) mkdir((dir))
#else
#define SSIZET_FMT "zd"
#define nsmkdir(dir, mode) mkdir((dir), (mode))
#endif

#if defined(__GNUC__) && (__GNUC__ < 3)
#define FLEX_ARRAY_LEN_DECL 0
#else
#define FLEX_ARRAY_LEN_DECL 
#endif

#if defined(__HAIKU__) || defined(__BEOS__)
#define strtof(s,p) ((float)(strtod((s),(p))))
#endif

#if !defined(ceilf) && defined(__MINT__)
#define ceilf(x) (float)ceil((double)x)
#endif

/**
 * Calculate length of constant C string.
 *
 * \param  x a constant C string.
 * \return The length of C string without its terminator.
 */
#define SLEN(x) (sizeof((x)) - 1)


#ifndef timeradd
#define timeradd(a, aa, result)						\
	do {								\
		(result)->tv_sec = (a)->tv_sec + (aa)->tv_sec;		\
		(result)->tv_usec = (a)->tv_usec + (aa)->tv_usec;	\
		if ((result)->tv_usec >= 1000000) {			\
			++(result)->tv_sec;				\
			(result)->tv_usec -= 1000000;			\
		}							\
	} while (0)
#endif

#ifndef timersub
#define timersub(a, aa, result)						\
	do {								\
		(result)->tv_sec = (a)->tv_sec - (aa)->tv_sec;		\
		(result)->tv_usec = (a)->tv_usec - (aa)->tv_usec;	\
		if ((result)->tv_usec < 0) {				\
			--(result)->tv_sec;				\
			(result)->tv_usec += 1000000;			\
		}							\
	} while (0)
#endif



/**
 * Replace consecutive whitespace with a single space.
 *
 * @todo determine if squash_whitespace utf-8 safe and that it needs to be
 *
 * \param  s  source string
 * \return  heap allocated result, or NULL on memory exhaustion
 */
char * squash_whitespace(const char * s);

/**
 * returns a string without its underscores
 *
 * \param s The string to change.
 * \param replacespace true to insert a space where there was an underscore
 * \return The altered string
 */
char *remove_underscores(const char *s, bool replacespace);

/**
 * Converts NUL terminated UTF-8 encoded string s containing zero or more
 * spaces (char 32) or TABs (char 9) to non-breaking spaces
 * (0xC2 + 0xA0 in UTF-8 encoding).
 *
 * Caller needs to free() result.  Returns NULL in case of error.  No
 * checking is done on validness of the UTF-8 input string.
 */
char *cnv_space2nbsp(const char *s);

/**
 * Check if a directory exists.
 */
bool is_dir(const char *path);

/**
 * Compile a regular expression, handling errors.
 *
 * Parameters as for regcomp(), see man regex.
 */
nserror regcomp_wrapper(regex_t *preg, const char *regex, int cflags);

/**
 * Create a human redable representation of a size in bytes.
 *
 * Does a simple conversion which assumes the user speaks English.
 * The buffer returned is one of three static ones so may change each
 * time this call is made.  Don't store the buffer for later use.
 * It's done this way for convenience and to fight possible memory
 * leaks, it is not necessarily pretty.
 *
 * @todo This implementation is strange doe sit need
 * reconsidering?
 *
 * @param bytesize The size in bytes.
 * @return A human readable string representing the size.
 */
char *human_friendly_bytesize(unsigned long bytesize);

/**
 * Create an RFC 1123 compliant date string from a Unix timestamp
 *
 * \param t The timestamp to consider
 * \return Pointer to buffer containing string - invalidated by next call.
 */
const char *rfc1123_date(time_t t);

/**
 * Returns a number of centiseconds, that increases in real time, for the
 * purposes of measuring how long something takes in wall-clock terms.
 *
 * The implementation uses gettimeofday() for this.  Should the call
 * to gettimeofday() fail, it returns zero.
 *
 * \return number of centiseconds that increases monotonically
 */
unsigned int wallclock(void);

/**
 * Generate a string from one or more component elemnts separated with
 * a single value.
 *
 * This is similar in intent to the perl join function creating a
 * single delimited string from an array of several.
 *
 * @note If a string is allocated it must be freed by the caller.
 *
 * @param[in,out] str pointer to string pointer if this is NULL enough
 *                    storage will be allocated for the complete path.
 * @param[in,out] size The size of the space available if \a str not
 *                     NULL on input and if not NULL set to the total
 *                     output length on output.
 * @param[in] sep The character to separete the elemnts with.
 * @param[in] nelm The number of elements up to a maximum of 16.
 * @param[in] ap The elements of the path as string pointers.
 * @return NSERROR_OK and the complete path is written to str or error
 *         code on faliure.
 */
nserror vsnstrjoin(char **str, size_t *size, char sep, size_t nelm, va_list ap);

/**
 * Generate a string from one or more component elemnts separated with
 * a single value.
 *
 * This is similar in intent to the perl join function creating a
 * single delimited string from an array of several.
 *
 * @note If a string is allocated it must be freed by the caller.
 *
 * @param[in,out] str pointer to string pointer if this is NULL enough
 *                    storage will be allocated for the complete path.
 * @param[in,out] size The size of the space available if \a str not
 *                     NULL on input and if not NULL set to the total
 *                     output length on output.
 * @param[in] sep The character to separete the elemnts with.
 * @param[in] nelm The number of elements up to a maximum of 16.
 * @param[in] ... The elements of the path as string pointers.
 * @return NSERROR_OK and the complete path is written to str or error
 *         code on faliure.
 */
nserror snstrjoin(char **str, size_t *size, char sep, size_t nelm, ...);

/**
 * Comparison function for sorting directories.
 *
 * Correctly orders non zero-padded numerical parts.
 * ie. produces "file1, file2, file10" rather than "file1, file10, file2".
 *
 * d1	first directory entry
 * d2	second directory entry
 */
int dir_sort_alpha(const struct dirent **d1, const struct dirent **d2);

/* Platform specific functions */
void warn_user(const char *warning, const char *detail);

#endif
