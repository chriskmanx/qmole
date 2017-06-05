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
 * @file msgs.c
 * Diagnostic message routines.
 *
 * This file contains routines related to the display of warning and
 * error messages to the end user.
 *
 * @author Michael Jennings <mej@eterm.org>
 * $Revision: 1.11 $
 * $Date: 2004/07/23 21:38:39 $
 */

static const char __attribute__((unused)) cvs_ident[] = "$Id: msgs.c,v 1.11 2004/07/23 21:38:39 mej Exp $";

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include "libast_internal.h"

/**
 * Program name.
 *
 * The client program name as shown in warning/error messages.  It is
 * also used in the config file parser for magic number checking and
 * some built-in functions.  This variable must be set using the
 * libast_set_program_name() function.
 */
spif_charptr_t libast_program_name = SPIF_CAST(charptr) PACKAGE;
/**
 * Program version.
 *
 * The version of the client program as shown in warning/error
 * messages.  It is also used in the config file parser for magic
 * number checking and some built-in functions.  This variable must be
 * set using the libast_set_program_version() function.
 */
spif_charptr_t libast_program_version = SPIF_CAST(charptr) VERSION;

/**
 * Sets the program name.
 *
 * This function is provided for safe and sane setting of the
 * #libast_program_name variable.  Setting it directly should be
 * avoided.
 *
 * @param progname The name of the client program as it should be
 *                 displayed in warning/error messages.
 * @see libast_program_name
 */
void
libast_set_program_name(const char *progname)
{
    if (libast_program_name) {
        if (!strcmp(SPIF_CAST_C(char *) libast_program_name, progname)) {
            return;
        }
        if (strcmp(SPIF_CAST_C(char *) libast_program_name, PACKAGE)) {
            FREE(libast_program_name);
        }
    }
    if (progname) {
        libast_program_name = SPIF_CAST(charptr) STRDUP(progname);
    } else {
        libast_program_name = SPIF_CAST(charptr) PACKAGE;
    }
}

/**
 * Sets the program version.
 *
 * This function is provided for safe and sane setting of the
 * #libast_program_version variable.  Setting it directly should be
 * avoided. 
 *
 * @param progversion The version of the client program as it should
 *                    appear in config file magic numbers and such.
 * @see libast_program_version
 */
void
libast_set_program_version(const char *progversion)
{
    if (libast_program_version) {
        if (!strcmp(SPIF_CAST_C(char *) libast_program_version, progversion)) {
            return;
        }
        if (strcmp(SPIF_CAST_C(char *) libast_program_version, VERSION)) {
            FREE(libast_program_version);
        }
    }
    if (progversion) {
        libast_program_version = SPIF_CAST(charptr) STRDUP(progversion);
    } else {
        libast_program_version = SPIF_CAST(charptr) VERSION;
    }
}

/**
 * Prints debugging output.
 *
 * This function is the guts behing the D_*() and DPRINTF() families of
 * macros.  Debugging output is sent to LIBAST_DEBUG_FD which is
 * flushed after each line in case of crash.
 *
 * @param format The printf-style format string.
 * @param ...    Zero or more parameters as required by the @a format
 *               string.
 * @return       The total number of characters printed.
 *
 * @see LIBAST_DEBUG_FD, DPRINTF()
 * @ingroup DOXGRP_DEBUG
 */
int
libast_dprintf(const char *format, ...)
{
    va_list args;
    int n;

    ASSERT_RVAL(!SPIF_PTR_ISNULL(format), SPIF_CAST_C(int) -1);
    REQUIRE_RVAL(libast_program_name != NULL, 0);
    va_start(args, format);
    n = vfprintf(LIBAST_DEBUG_FD, format, args);
    va_end(args);
    fflush(LIBAST_DEBUG_FD);
    return (n);
}

/**
 * Prints a non-terminal error message.
 *
 * This function displays a non-fatal error condition in the format
 * "<prog>:  Error:  <msg>".  Trailing newlines must be supplied by
 * the caller.
 *
 * @param fmt The printf-style format string.
 * @param ... Zero or more parameters as required by the @a fmt
 *            string.
 * @return    The total number of characters printed.
 *
 * @see libast_program_name
 */
void
libast_print_error(const char *fmt, ...)
{
    va_list arg_ptr;

    ASSERT(!SPIF_PTR_ISNULL(fmt));
    REQUIRE(libast_program_name != NULL);
    va_start(arg_ptr, fmt);
    fprintf(stderr, "%s:  Error:  ", libast_program_name);
    vfprintf(stderr, fmt, arg_ptr);
    va_end(arg_ptr);
}

/**
 * Prints a warning message.
 *
 * This function displays a warning message in the format
 * "<prog>:  Warning:  <msg>".  Trailing newlines must be supplied by
 * the caller.
 *
 * @param fmt The printf-style format string.
 * @param ... Zero or more parameters as required by the @a fmt
 *            string.
 * @return    The total number of characters printed.
 *
 * @see libast_program_name
 */
void
libast_print_warning(const char *fmt, ...)
{
    va_list arg_ptr;

    ASSERT(!SPIF_PTR_ISNULL(fmt));
    REQUIRE(libast_program_name != NULL);
    va_start(arg_ptr, fmt);
    fprintf(stderr, "%s:  Warning:  ", libast_program_name);
    vfprintf(stderr, fmt, arg_ptr);
    va_end(arg_ptr);
}

/**
 * Prints a terminal error message.
 *
 * This function displays a fatal error condition in the format
 * "<prog>:  FATAL:  <msg>".  Trailing newlines must be supplied by
 * the caller.  The program exits after the message is displayed.
 *
 * @param fmt The printf-style format string.
 * @param ... Zero or more parameters as required by the @a fmt
 *            string.
 * @return    Does not return.
 *
 * @see libast_program_name
 */
void
libast_fatal_error(const char *fmt, ...)
{
    va_list arg_ptr;

    ASSERT(!SPIF_PTR_ISNULL(fmt));
    if (libast_program_name != NULL) {
        va_start(arg_ptr, fmt);
        fprintf(stderr, "%s:  FATAL:  ", libast_program_name);
        vfprintf(stderr, fmt, arg_ptr);
        va_end(arg_ptr);
    }
    exit(-1);
}
